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
