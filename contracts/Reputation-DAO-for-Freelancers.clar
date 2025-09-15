(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATING (err u101))
(define-constant ERR-INSUFFICIENT-STAKE (err u102))
(define-constant ERR-ALREADY-RATED (err u103))
(define-constant ERR-NOT-FOUND (err u104))

(define-fungible-token reputation-token)

(define-non-fungible-token freelancer-profile uint)

(define-map freelancers
  { id: uint }
  {
    address: principal,
    total-ratings: uint,
    average-rating: uint,
    stake-balance: uint,
    status: (string-ascii 20)
  }
)

(define-map ratings
  { freelancer-id: uint, rater: principal }
  {
    rating: uint,
    stake-amount: uint,
    timestamp: uint
  }
)

(define-data-var next-freelancer-id uint u1)
(define-data-var min-stake-amount uint u100)
(define-data-var premium-threshold uint u80)

(define-public (register-freelancer)
  (let
    (
      (freelancer-id (var-get next-freelancer-id))
    )
    (try! (nft-mint? freelancer-profile freelancer-id tx-sender))
    (map-set freelancers
      { id: freelancer-id }
      {
        address: tx-sender,
        total-ratings: u0,
        average-rating: u0,
        stake-balance: u0,
        status: "active"
      }
    )
    (var-set next-freelancer-id (+ freelancer-id u1))
    (ok freelancer-id)
  )
)

(define-public (submit-rating (freelancer-id uint) (rating uint) (stake-amount uint))
  (let
    (
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
      (existing-rating (map-get? ratings { freelancer-id: freelancer-id, rater: tx-sender }))
    )
    (asserts! (and (>= rating u1) (<= rating u100)) ERR-INVALID-RATING)
    (asserts! (>= stake-amount (var-get min-stake-amount)) ERR-INSUFFICIENT-STAKE)
    (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
    
    (try! (ft-transfer? reputation-token stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set ratings
      { freelancer-id: freelancer-id, rater: tx-sender }
      {
        rating: rating,
        stake-amount: stake-amount,
        timestamp: burn-block-height
      }
    )
    
    (map-set freelancers
      { id: freelancer-id }
      {
        address: (get address freelancer),
        total-ratings: (+ (get total-ratings freelancer) u1),
        average-rating: (calculate-new-average freelancer-id rating),
        stake-balance: (+ (get stake-balance freelancer) stake-amount),
        status: (get status freelancer)
      }
    )
    (ok true)
  )
)

(define-read-only (get-freelancer-details (freelancer-id uint))
  (map-get? freelancers { id: freelancer-id })
)

(define-read-only (get-rating-details (freelancer-id uint) (rater principal))
  (map-get? ratings { freelancer-id: freelancer-id, rater: rater })
)

(define-read-only (is-premium-freelancer (freelancer-id uint))
  (let
    (
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
    )
    (ok (>= (get average-rating freelancer) (var-get premium-threshold)))
  )
)

(define-private (calculate-new-average (freelancer-id uint) (new-rating uint))
  (let
    (
      (freelancer (unwrap-panic (map-get? freelancers { id: freelancer-id })))
      (current-total (get total-ratings freelancer))
      (current-average (get average-rating freelancer))
    )
    (if (is-eq current-total u0)
      new-rating
      (/ (+ (* current-average current-total) new-rating) (+ current-total u1))
    )
  )
)

(define-constant ERR-DISPUTE-NOT-FOUND (err u200))
(define-constant ERR-DISPUTE-ALREADY-EXISTS (err u201))
(define-constant ERR-VOTING-PERIOD-ENDED (err u202))
(define-constant ERR-ALREADY-VOTED (err u203))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u204))

(define-data-var next-dispute-id uint u1)
(define-data-var voting-period-blocks uint u144)
(define-data-var min-reputation-to-vote uint u500)

(define-map disputes
  { id: uint }
  {
    freelancer-id: uint,
    disputed-rater: principal,
    reason: (string-ascii 200),
    status: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    created-at: uint,
    resolved-at: (optional uint)
  }
)

(define-map dispute-votes
  { dispute-id: uint, voter: principal }
  { vote: bool, reputation-weight: uint }
)

(define-public (submit-dispute (freelancer-id uint) (disputed-rater principal) (reason (string-ascii 200)))
  (let
    (
      (dispute-id (var-get next-dispute-id))
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
      (rating (unwrap! (map-get? ratings { freelancer-id: freelancer-id, rater: disputed-rater }) ERR-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get address freelancer)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? disputes { id: dispute-id })) ERR-DISPUTE-ALREADY-EXISTS)
    
    (map-set disputes
      { id: dispute-id }
      {
        freelancer-id: freelancer-id,
        disputed-rater: disputed-rater,
        reason: reason,
        status: "active",
        votes-for: u0,
        votes-against: u0,
        created-at: burn-block-height,
        resolved-at: none
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote-for bool))
  (let
    (
      (dispute (unwrap! (map-get? disputes { id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
      (voter-reputation (ft-get-balance reputation-token tx-sender))
      (existing-vote (map-get? dispute-votes { dispute-id: dispute-id, voter: tx-sender }))
    )
    (asserts! (>= voter-reputation (var-get min-reputation-to-vote)) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (< burn-block-height (+ (get created-at dispute) (var-get voting-period-blocks))) ERR-VOTING-PERIOD-ENDED)
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (is-eq (get status dispute) "active") ERR-DISPUTE-NOT-FOUND)
    
    (map-set dispute-votes
      { dispute-id: dispute-id, voter: tx-sender }
      { vote: vote-for, reputation-weight: voter-reputation }
    )
    
    (map-set disputes
      { id: dispute-id }
      (merge dispute {
        votes-for: (if vote-for (+ (get votes-for dispute) voter-reputation) (get votes-for dispute)),
        votes-against: (if vote-for (get votes-against dispute) (+ (get votes-against dispute) voter-reputation))
      })
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let
    (
      (dispute (unwrap! (map-get? disputes { id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
      (votes-for (get votes-for dispute))
      (votes-against (get votes-against dispute))
    )
    (asserts! (>= burn-block-height (+ (get created-at dispute) (var-get voting-period-blocks))) ERR-VOTING-PERIOD-ENDED)
    (asserts! (is-eq (get status dispute) "active") ERR-DISPUTE-NOT-FOUND)
    
    (if (> votes-for votes-against)
      (begin
        (try! (remove-disputed-rating (get freelancer-id dispute) (get disputed-rater dispute)))
        (map-set disputes { id: dispute-id } (merge dispute { status: "resolved-favor", resolved-at: (some burn-block-height) }))
      )
      (map-set disputes { id: dispute-id } (merge dispute { status: "resolved-against", resolved-at: (some burn-block-height) }))
    )
    
    (ok true)
  )
)

(define-private (remove-disputed-rating (freelancer-id uint) (disputed-rater principal))
  (let
    (
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
      (rating (unwrap! (map-get? ratings { freelancer-id: freelancer-id, rater: disputed-rater }) ERR-NOT-FOUND))
    )
    (map-delete ratings { freelancer-id: freelancer-id, rater: disputed-rater })
    (map-set freelancers
      { id: freelancer-id }
      (merge freelancer {
        total-ratings: (- (get total-ratings freelancer) u1),
        stake-balance: (- (get stake-balance freelancer) (get stake-amount rating))
      })
    )
    (ft-transfer? reputation-token (get stake-amount rating) (as-contract tx-sender) disputed-rater)
  )
)

(define-read-only (get-dispute-details (dispute-id uint))
  (map-get? disputes { id: dispute-id })
)

(define-constant ERR-SKILL-NOT-FOUND (err u300))
(define-constant ERR-SKILL-ALREADY-EXISTS (err u301))
(define-constant ERR-MAX-SKILLS-REACHED (err u302))
(define-constant ERR-SKILL-NOT-REGISTERED (err u303))

(define-data-var next-skill-id uint u1)
(define-data-var max-skills-per-freelancer uint u10)

(define-map skills
  { id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    created-by: principal,
    total-freelancers: uint
  }
)

(define-map freelancer-skills
  { freelancer-id: uint, skill-id: uint }
  {
    proficiency-level: uint,
    skill-rating: uint,
    skill-rating-count: uint,
    verified: bool
  }
)

(define-map skill-ratings
  { freelancer-id: uint, skill-id: uint, rater: principal }
  {
    rating: uint,
    feedback: (string-ascii 200),
    timestamp: uint
  }
)

(define-public (create-skill (name (string-ascii 50)) (category (string-ascii 30)))
  (let
    (
      (skill-id (var-get next-skill-id))
    )
    (map-set skills
      { id: skill-id }
      {
        name: name,
        category: category,
        created-by: tx-sender,
        total-freelancers: u0
      }
    )
    
    (var-set next-skill-id (+ skill-id u1))
    (ok skill-id)
  )
)

(define-public (register-freelancer-skill (freelancer-id uint) (skill-id uint) (proficiency-level uint))
  (let
    (
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
      (skill (unwrap! (map-get? skills { id: skill-id }) ERR-SKILL-NOT-FOUND))
      (existing-skill (map-get? freelancer-skills { freelancer-id: freelancer-id, skill-id: skill-id }))
      (freelancer-skill-count (get-freelancer-skill-count freelancer-id))
    )
    (asserts! (is-eq tx-sender (get address freelancer)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= proficiency-level u1) (<= proficiency-level u5)) ERR-INVALID-RATING)
    (asserts! (is-none existing-skill) ERR-SKILL-ALREADY-EXISTS)
    (asserts! (< freelancer-skill-count (var-get max-skills-per-freelancer)) ERR-MAX-SKILLS-REACHED)
    
    (map-set freelancer-skills
      { freelancer-id: freelancer-id, skill-id: skill-id }
      {
        proficiency-level: proficiency-level,
        skill-rating: u0,
        skill-rating-count: u0,
        verified: false
      }
    )
    
    (map-set skills
      { id: skill-id }
      (merge skill { total-freelancers: (+ (get total-freelancers skill) u1) })
    )
    
    (ok true)
  )
)

(define-public (rate-freelancer-skill (freelancer-id uint) (skill-id uint) (rating uint) (feedback (string-ascii 200)))
  (let
    (
      (freelancer-skill (unwrap! (map-get? freelancer-skills { freelancer-id: freelancer-id, skill-id: skill-id }) ERR-SKILL-NOT-REGISTERED))
      (existing-rating (map-get? skill-ratings { freelancer-id: freelancer-id, skill-id: skill-id, rater: tx-sender }))
    )
    (asserts! (and (>= rating u1) (<= rating u100)) ERR-INVALID-RATING)
    (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
    
    (map-set skill-ratings
      { freelancer-id: freelancer-id, skill-id: skill-id, rater: tx-sender }
      {
        rating: rating,
        feedback: feedback,
        timestamp: burn-block-height
      }
    )
    
    (let
      (
        (current-count (get skill-rating-count freelancer-skill))
        (current-rating (get skill-rating freelancer-skill))
        (new-average (if (is-eq current-count u0)
                      rating
                      (/ (+ (* current-rating current-count) rating) (+ current-count u1))))
      )
      (map-set freelancer-skills
        { freelancer-id: freelancer-id, skill-id: skill-id }
        (merge freelancer-skill {
          skill-rating: new-average,
          skill-rating-count: (+ current-count u1)
        })
      )
    )
    
    (ok true)
  )
)

(define-public (verify-freelancer-skill (freelancer-id uint) (skill-id uint))
  (let
    (
      (freelancer-skill (unwrap! (map-get? freelancer-skills { freelancer-id: freelancer-id, skill-id: skill-id }) ERR-SKILL-NOT-REGISTERED))
      (voter-reputation (ft-get-balance reputation-token tx-sender))
    )
    (asserts! (>= voter-reputation (var-get min-reputation-to-vote)) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (>= (get skill-rating freelancer-skill) u75) ERR-INVALID-RATING)
    (asserts! (>= (get skill-rating-count freelancer-skill) u5) ERR-INSUFFICIENT-STAKE)
    
    (map-set freelancer-skills
      { freelancer-id: freelancer-id, skill-id: skill-id }
      (merge freelancer-skill { verified: true })
    )
    
    (ok true)
  )
)

(define-read-only (get-skill-details (skill-id uint))
  (map-get? skills { id: skill-id })
)

(define-read-only (get-freelancer-skill (freelancer-id uint) (skill-id uint))
  (map-get? freelancer-skills { freelancer-id: freelancer-id, skill-id: skill-id })
)

(define-read-only (get-skill-rating (freelancer-id uint) (skill-id uint) (rater principal))
  (map-get? skill-ratings { freelancer-id: freelancer-id, skill-id: skill-id, rater: rater })
)

(define-private (get-freelancer-skill-count (freelancer-id uint))
  u0
)

(define-constant ERR-ESCROW-NOT-FOUND (err u400))
(define-constant ERR-ESCROW-ALREADY-EXISTS (err u401))
(define-constant ERR-INVALID-MILESTONE (err u402))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u403))
(define-constant ERR-ESCROW-NOT-ACTIVE (err u404))
(define-constant ERR-INSUFFICIENT-BALANCE (err u405))

(define-data-var next-escrow-id uint u1)

(define-map escrows
  { id: uint }
  {
    client: principal,
    freelancer: principal,
    total-amount: uint,
    released-amount: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map milestones
  { escrow-id: uint, milestone-index: uint }
  {
    description: (string-ascii 200),
    amount: uint,
    completed: bool,
    approved-by-client: bool,
    completion-date: (optional uint)
  }
)

(define-map escrow-milestone-count
  { escrow-id: uint }
  { count: uint }
)

(define-public (create-escrow (freelancer principal) (milestone-amount uint) (milestone-description (string-ascii 200)))
  (let
    (
      (escrow-id (var-get next-escrow-id))
    )
    (asserts! (> milestone-amount u0) ERR-INVALID-MILESTONE)
    (asserts! (>= (ft-get-balance reputation-token tx-sender) milestone-amount) ERR-INSUFFICIENT-BALANCE)
    
    (try! (ft-transfer? reputation-token milestone-amount tx-sender (as-contract tx-sender)))
    
    (map-set escrows
      { id: escrow-id }
      {
        client: tx-sender,
        freelancer: freelancer,
        total-amount: milestone-amount,
        released-amount: u0,
        status: "active",
        created-at: burn-block-height,
        completed-at: none
      }
    )
    
    (map-set escrow-milestone-count
      { escrow-id: escrow-id }
      { count: u1 }
    )
    
    (unwrap-panic (create-single-milestone escrow-id u0 milestone-amount milestone-description))
    
    (var-set next-escrow-id (+ escrow-id u1))
    (ok escrow-id)
  )
)

(define-private (create-single-milestone (escrow-id uint) (milestone-index uint) (amount uint) (description (string-ascii 200)))
  (begin
    (map-set milestones
      { escrow-id: escrow-id, milestone-index: milestone-index }
      {
        description: description,
        amount: amount,
        completed: false,
        approved-by-client: false,
        completion-date: none
      }
    )
    (ok true)
  )
)

(define-public (complete-milestone (escrow-id uint) (milestone-index uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (milestone (unwrap! (map-get? milestones { escrow-id: escrow-id, milestone-index: milestone-index }) ERR-INVALID-MILESTONE))
    )
    (asserts! (is-eq tx-sender (get freelancer escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "active") ERR-ESCROW-NOT-ACTIVE)
    (asserts! (not (get completed milestone)) ERR-MILESTONE-ALREADY-COMPLETED)
    
    (map-set milestones
      { escrow-id: escrow-id, milestone-index: milestone-index }
      (merge milestone {
        completed: true,
        completion-date: (some burn-block-height)
      })
    )
    
    (ok true)
  )
)

(define-public (approve-milestone (escrow-id uint) (milestone-index uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (milestone (unwrap! (map-get? milestones { escrow-id: escrow-id, milestone-index: milestone-index }) ERR-INVALID-MILESTONE))
    )
    (asserts! (is-eq tx-sender (get client escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "active") ERR-ESCROW-NOT-ACTIVE)
    (asserts! (get completed milestone) ERR-INVALID-MILESTONE)
    (asserts! (not (get approved-by-client milestone)) ERR-MILESTONE-ALREADY-COMPLETED)
    
    (map-set milestones
      { escrow-id: escrow-id, milestone-index: milestone-index }
      (merge milestone { approved-by-client: true })
    )
    
    (try! (ft-transfer? reputation-token (get amount milestone) (as-contract tx-sender) (get freelancer escrow)))
    
    (map-set escrows
      { id: escrow-id }
      (merge escrow { released-amount: (+ (get released-amount escrow) (get amount milestone)) })
    )
    
    (if (is-eq (+ (get released-amount escrow) (get amount milestone)) (get total-amount escrow))
      (map-set escrows { id: escrow-id } (merge escrow { status: "completed", completed-at: (some burn-block-height) }))
      true
    )
    
    (ok true)
  )
)

(define-public (release-escrow-on-dispute (escrow-id uint) (dispute-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (dispute (unwrap! (map-get? disputes { id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
    )
    (asserts! (is-eq (get status dispute) "resolved-favor") ERR-DISPUTE-NOT-FOUND)
    (asserts! (is-eq (get status escrow) "active") ERR-ESCROW-NOT-ACTIVE)
    
    (let
      (
        (remaining-amount (- (get total-amount escrow) (get released-amount escrow)))
      )
      (try! (ft-transfer? reputation-token remaining-amount (as-contract tx-sender) (get freelancer escrow)))
      
      (map-set escrows
        { id: escrow-id }
        (merge escrow {
          released-amount: (get total-amount escrow),
          status: "completed",
          completed-at: (some burn-block-height)
        })
      )
    )
    
    (ok true)
  )
)

(define-read-only (get-escrow-details (escrow-id uint))
  (map-get? escrows { id: escrow-id })
)

(define-read-only (get-milestone-details (escrow-id uint) (milestone-index uint))
  (map-get? milestones { escrow-id: escrow-id, milestone-index: milestone-index })
)

(define-read-only (get-escrow-milestone-count (escrow-id uint))
  (map-get? escrow-milestone-count { escrow-id: escrow-id })
)

(define-constant ERR-REWARD-ALREADY-CLAIMED (err u500))
(define-constant ERR-INSUFFICIENT-PERFORMANCE (err u501))
(define-constant ERR-REWARD-COOLDOWN-ACTIVE (err u502))
(define-constant ERR-NO-REWARDS-AVAILABLE (err u503))

(define-data-var rating-milestone-reward uint u50)
(define-data-var completion-bonus-reward uint u100)
(define-data-var premium-status-reward uint u200)
(define-data-var reward-cooldown-blocks uint u1008)

(define-map user-rewards
  { user: principal }
  {
    total-earned: uint,
    last-claim-block: uint,
    rating-milestones-claimed: uint,
    completion-bonuses-claimed: uint,
    premium-rewards-claimed: uint
  }
)

(define-map achievement-milestones
  { user: principal, milestone-type: (string-ascii 20), milestone-value: uint }
  {
    achieved: bool,
    achieved-at: uint,
    reward-amount: uint,
    claimed: bool
  }
)

(define-map reward-distribution-history
  { user: principal, block-height: uint }
  {
    reward-type: (string-ascii 20),
    amount: uint,
    trigger-event: (string-ascii 50)
  }
)

(define-public (claim-rating-milestone-reward (freelancer-id uint))
  (let
    (
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
      (user-reward (default-to 
        { total-earned: u0, last-claim-block: u0, rating-milestones-claimed: u0, completion-bonuses-claimed: u0, premium-rewards-claimed: u0 }
        (map-get? user-rewards { user: tx-sender })
      ))
      (total-ratings (get total-ratings freelancer))
      (average-rating (get average-rating freelancer))
    )
    (asserts! (is-eq tx-sender (get address freelancer)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (- burn-block-height (get last-claim-block user-reward)) (var-get reward-cooldown-blocks)) ERR-REWARD-COOLDOWN-ACTIVE)
    (asserts! (and (>= total-ratings u10) (>= average-rating u75)) ERR-INSUFFICIENT-PERFORMANCE)
    
    (let
      (
        (reward-amount (var-get rating-milestone-reward))
        (milestone-key { user: tx-sender, milestone-type: "rating", milestone-value: total-ratings })
        (existing-milestone (map-get? achievement-milestones milestone-key))
      )
      (asserts! (or (is-none existing-milestone) (not (get claimed (unwrap-panic existing-milestone)))) ERR-REWARD-ALREADY-CLAIMED)
      
      (try! (ft-mint? reputation-token reward-amount tx-sender))
      
      (map-set user-rewards
        { user: tx-sender }
        {
          total-earned: (+ (get total-earned user-reward) reward-amount),
          last-claim-block: burn-block-height,
          rating-milestones-claimed: (+ (get rating-milestones-claimed user-reward) u1),
          completion-bonuses-claimed: (get completion-bonuses-claimed user-reward),
          premium-rewards-claimed: (get premium-rewards-claimed user-reward)
        }
      )
      
      (map-set achievement-milestones
        milestone-key
        {
          achieved: true,
          achieved-at: burn-block-height,
          reward-amount: reward-amount,
          claimed: true
        }
      )
      
      (map-set reward-distribution-history
        { user: tx-sender, block-height: burn-block-height }
        {
          reward-type: "rating-milestone",
          amount: reward-amount,
          trigger-event: "high-rating-achievement"
        }
      )
      
      (ok reward-amount)
    )
  )
)

(define-public (claim-completion-bonus (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrows { id: escrow-id }) ERR-ESCROW-NOT-FOUND))
      (user-reward (default-to 
        { total-earned: u0, last-claim-block: u0, rating-milestones-claimed: u0, completion-bonuses-claimed: u0, premium-rewards-claimed: u0 }
        (map-get? user-rewards { user: tx-sender })
      ))
    )
    (asserts! (is-eq tx-sender (get freelancer escrow)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status escrow) "completed") ERR-ESCROW-NOT-ACTIVE)
    (asserts! (>= (- burn-block-height (get last-claim-block user-reward)) (var-get reward-cooldown-blocks)) ERR-REWARD-COOLDOWN-ACTIVE)
    
    (let
      (
        (reward-amount (var-get completion-bonus-reward))
        (milestone-key { user: tx-sender, milestone-type: "completion", milestone-value: escrow-id })
        (existing-milestone (map-get? achievement-milestones milestone-key))
      )
      (asserts! (or (is-none existing-milestone) (not (get claimed (unwrap-panic existing-milestone)))) ERR-REWARD-ALREADY-CLAIMED)
      
      (try! (ft-mint? reputation-token reward-amount tx-sender))
      
      (map-set user-rewards
        { user: tx-sender }
        {
          total-earned: (+ (get total-earned user-reward) reward-amount),
          last-claim-block: burn-block-height,
          rating-milestones-claimed: (get rating-milestones-claimed user-reward),
          completion-bonuses-claimed: (+ (get completion-bonuses-claimed user-reward) u1),
          premium-rewards-claimed: (get premium-rewards-claimed user-reward)
        }
      )
      
      (map-set achievement-milestones
        milestone-key
        {
          achieved: true,
          achieved-at: burn-block-height,
          reward-amount: reward-amount,
          claimed: true
        }
      )
      
      (map-set reward-distribution-history
        { user: tx-sender, block-height: burn-block-height }
        {
          reward-type: "completion-bonus",
          amount: reward-amount,
          trigger-event: "project-completion"
        }
      )
      
      (ok reward-amount)
    )
  )
)

(define-public (claim-premium-status-reward (freelancer-id uint))
  (let
    (
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
      (user-reward (default-to 
        { total-earned: u0, last-claim-block: u0, rating-milestones-claimed: u0, completion-bonuses-claimed: u0, premium-rewards-claimed: u0 }
        (map-get? user-rewards { user: tx-sender })
      ))
      (is-premium (unwrap! (is-premium-freelancer freelancer-id) ERR-INSUFFICIENT-PERFORMANCE))
    )
    (asserts! (is-eq tx-sender (get address freelancer)) ERR-NOT-AUTHORIZED)
    (asserts! is-premium ERR-INSUFFICIENT-PERFORMANCE)
    (asserts! (>= (- burn-block-height (get last-claim-block user-reward)) (var-get reward-cooldown-blocks)) ERR-REWARD-COOLDOWN-ACTIVE)
    
    (let
      (
        (reward-amount (var-get premium-status-reward))
        (milestone-key { user: tx-sender, milestone-type: "premium", milestone-value: freelancer-id })
        (existing-milestone (map-get? achievement-milestones milestone-key))
      )
      (asserts! (or (is-none existing-milestone) (not (get claimed (unwrap-panic existing-milestone)))) ERR-REWARD-ALREADY-CLAIMED)
      
      (try! (ft-mint? reputation-token reward-amount tx-sender))
      
      (map-set user-rewards
        { user: tx-sender }
        {
          total-earned: (+ (get total-earned user-reward) reward-amount),
          last-claim-block: burn-block-height,
          rating-milestones-claimed: (get rating-milestones-claimed user-reward),
          completion-bonuses-claimed: (get completion-bonuses-claimed user-reward),
          premium-rewards-claimed: (+ (get premium-rewards-claimed user-reward) u1)
        }
      )
      
      (map-set achievement-milestones
        milestone-key
        {
          achieved: true,
          achieved-at: burn-block-height,
          reward-amount: reward-amount,
          claimed: true
        }
      )
      
      (map-set reward-distribution-history
        { user: tx-sender, block-height: burn-block-height }
        {
          reward-type: "premium-status",
          amount: reward-amount,
          trigger-event: "premium-status-achieved"
        }
      )
      
      (ok reward-amount)
    )
  )
)

(define-read-only (get-user-rewards (user principal))
  (map-get? user-rewards { user: user })
)

(define-read-only (get-achievement-milestone (user principal) (milestone-type (string-ascii 20)) (milestone-value uint))
  (map-get? achievement-milestones { user: user, milestone-type: milestone-type, milestone-value: milestone-value })
)

(define-read-only (get-reward-history (user principal) (block-number uint))
  (map-get? reward-distribution-history { user: user, block-height: block-number })
)

(define-read-only (calculate-pending-rewards (freelancer-id uint))
  (let
    (
      (freelancer (unwrap! (map-get? freelancers { id: freelancer-id }) ERR-NOT-FOUND))
      (user-reward (default-to 
        { total-earned: u0, last-claim-block: u0, rating-milestones-claimed: u0, completion-bonuses-claimed: u0, premium-rewards-claimed: u0 }
        (map-get? user-rewards { user: (get address freelancer) })
      ))
      (total-ratings (get total-ratings freelancer))
      (average-rating (get average-rating freelancer))
      (is-premium (unwrap! (is-premium-freelancer freelancer-id) ERR-NOT-FOUND))
      (cooldown-passed (>= (- burn-block-height (get last-claim-block user-reward)) (var-get reward-cooldown-blocks)))
    )
    (ok {
      rating-milestone-eligible: (and cooldown-passed (>= total-ratings u10) (>= average-rating u75)),
      premium-status-eligible: (and cooldown-passed is-premium),
      potential-rating-reward: (if (and cooldown-passed (>= total-ratings u10) (>= average-rating u75)) (var-get rating-milestone-reward) u0),
      potential-premium-reward: (if (and cooldown-passed is-premium) (var-get premium-status-reward) u0),
      blocks-until-next-claim: (if cooldown-passed u0 (- (var-get reward-cooldown-blocks) (- burn-block-height (get last-claim-block user-reward))))
    })
  )
)

(define-read-only (get-reward-system-config)
  (ok {
    rating-milestone-reward: (var-get rating-milestone-reward),
    completion-bonus-reward: (var-get completion-bonus-reward),
    premium-status-reward: (var-get premium-status-reward),
    reward-cooldown-blocks: (var-get reward-cooldown-blocks)
  })
)
