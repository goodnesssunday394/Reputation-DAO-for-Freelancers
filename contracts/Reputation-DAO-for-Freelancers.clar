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