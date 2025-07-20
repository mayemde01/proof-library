;; farmo-participants
;; 
;; This contract manages the registration, verification, and permissions for all participants
;; in the Farmo agricultural supply chain ecosystem. It establishes a trusted network of verified
;; participants (farmers, distributors, processors, retailers) and maintains their reputation
;; scores and authorization status. The contract serves as the foundation for ensuring data
;; integrity and accountability throughout the entire supply chain.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-REGISTERED (err u101))
(define-constant ERR-PARTICIPANT-NOT-FOUND (err u102))
(define-constant ERR-INVALID-ROLE (err u103))
(define-constant ERR-INVALID-STATUS (err u104))
(define-constant ERR-ONLY-ADMIN (err u105))
(define-constant ERR-ONLY-VERIFIER (err u106))
(define-constant ERR-INVALID-REPUTATION-SCORE (err u107))

;; Constants
(define-constant ROLE-FARMER u1)
(define-constant ROLE-DISTRIBUTOR u2)
(define-constant ROLE-PROCESSOR u3)
(define-constant ROLE-RETAILER u4)
(define-constant ROLE-VERIFIER u5)
(define-constant ROLE-ADMIN u6)

(define-constant STATUS-PENDING u1)
(define-constant STATUS-VERIFIED u2)
(define-constant STATUS-SUSPENDED u3)
(define-constant STATUS-REVOKED u4)

;; Data storage

;; Track the contract owner/administrator who has special permissions
(define-data-var contract-owner principal tx-sender)

;; Store participant information
(define-map participants principal 
  {
    id: principal,
    name: (string-utf8 100),
    role: uint,
    status: uint,
    reputation-score: uint,
    location: (string-utf8 100),
    registration-date: uint,
    last-updated: uint,
    verifier: (optional principal),
    metadata: (string-utf8 256)
  }
)

;; Track verifiers who can approve participants
(define-map verifiers principal bool)

;; Track admins who can manage contract settings
(define-map admins principal bool)

;; Keep count of participants by role
(define-map role-counts uint uint)

;; Private functions

;; Check if caller is an admin
(define-private (is-admin (caller principal))
  (default-to false (map-get? admins caller))
)

;; Check if caller is a verifier
(define-private (is-verifier (caller principal))
  (default-to false (map-get? verifiers caller))
)

;; Check if caller is the contract owner
(define-private (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner))
)

;; Validate role is one of the supported types
(define-private (is-valid-role (role uint))
  (or
    (is-eq role ROLE-FARMER)
    (is-eq role ROLE-DISTRIBUTOR)
    (is-eq role ROLE-PROCESSOR)
    (is-eq role ROLE-RETAILER)
    (is-eq role ROLE-VERIFIER)
    (is-eq role ROLE-ADMIN)
  )
)

;; Validate status is one of the supported types
(define-private (is-valid-status (status uint))
  (or
    (is-eq status STATUS-PENDING)
    (is-eq status STATUS-VERIFIED)
    (is-eq status STATUS-SUSPENDED)
    (is-eq status STATUS-REVOKED)
  )
)

;; Increment the count for a specific role
(define-private (increment-role-count (role uint))
  (map-set role-counts 
    role 
    (+ (default-to u0 (map-get? role-counts role)) u1)
  )
)

;; Decrement the count for a specific role
(define-private (decrement-role-count (role uint))
  (let ((current-count (default-to u0 (map-get? role-counts role))))
    (if (> current-count u0)
      (map-set role-counts role (- current-count u1))
      (map-set role-counts role u0)
    )
  )
)

;; Read-only functions

;; Get participant information by principal
(define-read-only (get-participant (participant-id principal))
  (map-get? participants participant-id)
)

;; Check if a participant exists
(define-read-only (participant-exists (participant-id principal))
  (is-some (map-get? participants participant-id))
)

;; Check if a participant is verified
(define-read-only (is-participant-verified (participant-id principal))
  (let ((participant (map-get? participants participant-id)))
    (if (is-some participant)
      (is-eq (get status (unwrap-panic participant)) STATUS-VERIFIED)
      false
    )
  )
)

;; Get total count of participants by role
(define-read-only (get-role-count (role uint))
  (default-to u0 (map-get? role-counts role))
)

;; Check if a participant is authorized for a specific role
(define-read-only (is-authorized-for-role (participant-id principal) (role uint))
  (let ((participant (map-get? participants participant-id)))
    (and
      (is-some participant)
      (is-eq (get role (unwrap-panic participant)) role)
      (is-eq (get status (unwrap-panic participant)) STATUS-VERIFIED)
    )
  )
)

;; Public functions

;; Register as a new participant (self-registration)
(define-public (register-participant
    (name (string-utf8 100))
    (role uint)
    (location (string-utf8 100))
    (metadata (string-utf8 256)))
  (let ((caller tx-sender)
        (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (asserts! (not (participant-exists caller)) ERR-ALREADY-REGISTERED)
    (asserts! (is-valid-role role) ERR-INVALID-ROLE)
    (asserts! (not (or (is-eq role ROLE-ADMIN) (is-eq role ROLE-VERIFIER))) ERR-NOT-AUTHORIZED)
    
    (map-set participants caller
      {
        id: caller,
        name: name,
        role: role,
        status: STATUS-PENDING,
        reputation-score: u0,
        location: location,
        registration-date: current-time,
        last-updated: current-time,
        verifier: none,
        metadata: metadata
      }
    )
    
    (increment-role-count role)
    (ok true)
  )
)

;; Verify a participant's registration
(define-public (verify-participant (participant-id principal))
  (let ((caller tx-sender)
        (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (asserts! (or (is-verifier caller) (is-admin caller)) ERR-ONLY-VERIFIER)
    (asserts! (participant-exists participant-id) ERR-PARTICIPANT-NOT-FOUND)
    
    (let ((participant (unwrap-panic (map-get? participants participant-id))))
      (asserts! (is-eq (get status participant) STATUS-PENDING) ERR-INVALID-STATUS)
      
      (map-set participants participant-id
        (merge participant 
          {
            status: STATUS-VERIFIED,
            last-updated: current-time,
            verifier: (some caller)
          }
        )
      )
      (ok true)
    )
  )
)

;; Update participant status (suspend, revoke, or reactivate)
(define-public (update-participant-status (participant-id principal) (new-status uint))
  (let ((caller tx-sender)
        (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (asserts! (or (is-verifier caller) (is-admin caller)) ERR-ONLY-VERIFIER)
    (asserts! (participant-exists participant-id) ERR-PARTICIPANT-NOT-FOUND)
    (asserts! (is-valid-status new-status) ERR-INVALID-STATUS)
    
    (let ((participant (unwrap-panic (map-get? participants participant-id))))
      (map-set participants participant-id
        (merge participant 
          {
            status: new-status,
            last-updated: current-time
          }
        )
      )
      (ok true)
    )
  )
)

;; Update participant information
(define-public (update-participant-info
    (name (string-utf8 100))
    (location (string-utf8 100))
    (metadata (string-utf8 256)))
  (let ((caller tx-sender)
        (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (asserts! (participant-exists caller) ERR-PARTICIPANT-NOT-FOUND)
    
    (let ((participant (unwrap-panic (map-get? participants caller))))
      (map-set participants caller
        (merge participant 
          {
            name: name,
            location: location,
            metadata: metadata,
            last-updated: current-time
          }
        )
      )
      (ok true)
    )
  )
)


;; Remove a verifier
(define-public (remove-verifier (verifier-id principal))
  (let ((caller tx-sender))
    (asserts! (is-admin caller) ERR-ONLY-ADMIN)
    (asserts! (is-verifier verifier-id) ERR-NOT-AUTHORIZED)
    
    (map-delete verifiers verifier-id)
    (decrement-role-count ROLE-VERIFIER)
    (ok true)
  )
)


;; Transfer contract ownership
(define-public (transfer-ownership (new-owner principal))
  (let ((caller tx-sender))
    (asserts! (is-contract-owner caller) ERR-ONLY-ADMIN)
    (var-set contract-owner new-owner)
    (ok true)
  )
)
