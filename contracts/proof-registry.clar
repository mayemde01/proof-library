;; farmo-registry.clar
;; This contract manages the registration and lifecycle tracking of agricultural products in the Farmo ecosystem.
;; It enables product registration, custody transfers, event tracking, and verification of certifications throughout
;; the agricultural supply chain, providing transparent and immutable records for all participants.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PRODUCT (err u101))
(define-constant ERR-PRODUCT-EXISTS (err u102))
(define-constant ERR-NOT-PRODUCT-OWNER (err u103))
(define-constant ERR-INVALID-HANDLER (err u104))
(define-constant ERR-PRODUCT-ALREADY-SOLD (err u105))
(define-constant ERR-UNKNOWN-CERTIFICATION-AUTHORITY (err u106))
(define-constant ERR-CERTIFICATION-NOT-FOUND (err u107))
(define-constant ERR-CERTIFICATION-ALREADY-EXISTS (err u108))

;; Data variables
(define-data-var product-counter uint u0)

;; Data maps
;; Registry of all products with basic information
(define-map products 
  { product-id: uint }
  {
    farm-id: principal,
    product-type: (string-ascii 50),
    harvest-date: uint,
    current-custodian: principal,
    status: (string-ascii 20),  ;; "active", "sold", "expired", etc.
    is-sold: bool
  }
)

;; Map storing certifications for products
(define-map product-certifications
  { product-id: uint, cert-type: (string-ascii 30) }
  {
    certifier: principal,
    issued-date: uint,
    expiry-date: uint,
    details: (string-utf8 200)
  }
)

;; Map storing the complete history of events for each product
(define-map product-history
  { product-id: uint, event-index: uint }
  {
    timestamp: uint,
    event-type: (string-ascii 30),
    handler: principal,
    details: (string-utf8 200),
    location: (optional (string-ascii 100))
  }
)

;; Map tracking the number of events per product for indexing
(define-map product-event-count
  { product-id: uint }
  { count: uint }
)

;; Map of authorized handlers in the supply chain
(define-map authorized-handlers
  { handler-id: principal }
  {
    handler-type: (string-ascii 30),  ;; "distributor", "processor", "retailer", etc.
    is-active: bool,
    registration-date: uint
  }
)

;; Map of authorized certification authorities
(define-map certification-authorities
  { authority-id: principal }
  {
    authority-name: (string-ascii 50),
    certification-types: (list 10 (string-ascii 30)),
    is-active: bool
  }
)

;; Private Functions

;; Get a new unique product ID
(define-private (get-next-product-id)
  (let ((current-id (var-get product-counter)))
    (var-set product-counter (+ current-id u1))
    current-id
  )
)

;; Validate that a principal is an authorized handler
(define-private (is-authorized-handler (handler principal))
  (match (map-get? authorized-handlers { handler-id: handler })
    handler-data (and (get is-active handler-data) true)
    false
  )
)

;; Validate that a principal is the current custodian of a product
(define-private (is-current-custodian (product-id uint) (principal-to-check principal))
  (match (map-get? products { product-id: product-id })
    product-data (is-eq (get current-custodian product-data) principal-to-check)
    false
  )
)

;; Add a new event to a product's history
(define-private (add-product-event (product-id uint) (event-type (string-ascii 30)) (details (string-utf8 200)) (location (optional (string-ascii 100))))
  (let (
    (event-count (default-to { count: u0 } (map-get? product-event-count { product-id: product-id })))
    (new-event-index (get count event-count))
  )
    ;; Update event count
    (map-set product-event-count 
      { product-id: product-id }
      { count: (+ new-event-index u1) }
    )
    
    ;; Add the new event
    (map-set product-history
      { product-id: product-id, event-index: new-event-index }
      {
        timestamp: block-height,
        event-type: event-type,
        handler: tx-sender,
        details: details,
        location: location
      }
    )
    (ok new-event-index)
  )
)

;; Check if a certification authority is authorized for a specific certification type
(define-private (is-authorized-for-cert-type (authority principal) (cert-type (string-ascii 30)))
  (match (map-get? certification-authorities { authority-id: authority })
    authority-data (and 
                    (get is-active authority-data)
                    (is-some (index-of (get certification-types authority-data) cert-type))
                   )
    false
  )
)

;; Public Functions

;; Register a new handler in the supply chain
(define-public (register-handler (handler-type (string-ascii 30)))
  (begin
    (map-set authorized-handlers
      { handler-id: tx-sender }
      {
        handler-type: handler-type,
        is-active: true,
        registration-date: block-height
      }
    )
    (ok tx-sender)
  )
)

;; Register a new certification authority
(define-public (register-certification-authority (authority-name (string-ascii 50)) (certification-types (list 10 (string-ascii 30))))
  (begin
    (map-set certification-authorities
      { authority-id: tx-sender }
      {
        authority-name: authority-name,
        certification-types: certification-types,
        is-active: true
      }
    )
    (ok tx-sender)
  )
)

;; Register a new agricultural product
(define-public (register-product (product-type (string-ascii 50)) (harvest-date uint))
  (let (
    (new-id (get-next-product-id))
  )
    (map-set products
      { product-id: new-id }
      {
        farm-id: tx-sender,
        product-type: product-type,
        harvest-date: harvest-date,
        current-custodian: tx-sender,
        status: "active",
        is-sold: false
      }
    )
    
    ;; Initialize event count
    (map-set product-event-count 
      { product-id: new-id }
      { count: u0 }
    )
    
    ;; Create first history event - registration
    (add-product-event new-id "registration" (concat "Product registered: " product-type) none)
    
    (ok new-id)
  )
)

;; Transfer custody of a product to another handler
(define-public (transfer-custody (product-id uint) (new-custodian principal) (details (string-utf8 200)) (location (optional (string-ascii 100))))
  (let (
    (product (map-get? products { product-id: product-id }))
  )
    (asserts! (is-some product) ERR-INVALID-PRODUCT)
    (asserts! (is-current-custodian product-id tx-sender) ERR-NOT-PRODUCT-OWNER)
    (asserts! (is-authorized-handler new-custodian) ERR-INVALID-HANDLER)
    (asserts! (not (get is-sold (unwrap-panic product))) ERR-PRODUCT-ALREADY-SOLD)
    
    ;; Update custodian
    (map-set products
      { product-id: product-id }
      (merge (unwrap-panic product) { current-custodian: new-custodian })
    )
    
    ;; Record the transfer event
    (add-product-event product-id "custody-transfer" details location)
    
    (ok true)
  )
)

;; Add certification to a product
(define-public (add-certification (product-id uint) (cert-type (string-ascii 30)) (expiry-date uint) (details (string-utf8 200)))
  (let (
    (product (map-get? products { product-id: product-id }))
    (existing-cert (map-get? product-certifications { product-id: product-id, cert-type: cert-type }))
  )
    (asserts! (is-some product) ERR-INVALID-PRODUCT)
    (asserts! (is-authorized-for-cert-type tx-sender cert-type) ERR-UNKNOWN-CERTIFICATION-AUTHORITY)
    (asserts! (is-none existing-cert) ERR-CERTIFICATION-ALREADY-EXISTS)
    
    ;; Add the certification
    (map-set product-certifications
      { product-id: product-id, cert-type: cert-type }
      {
        certifier: tx-sender,
        issued-date: block-height,
        expiry-date: expiry-date,
        details: details
      }
    )
    
    ;; Record certification event
    (add-product-event product-id "certification" 
      (concat (concat "Added certification: " cert-type) (concat " - " details)) 
      none
    )
    
    (ok true)
  )
)

;; Record a supply chain event (processing, quality check, etc.)
(define-public (record-supply-chain-event (product-id uint) (event-type (string-ascii 30)) (details (string-utf8 200)) (location (optional (string-ascii 100))))
  (let (
    (product (map-get? products { product-id: product-id }))
  )
    (asserts! (is-some product) ERR-INVALID-PRODUCT)
    (asserts! (is-authorized-handler tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-current-custodian product-id tx-sender) ERR-NOT-PRODUCT-OWNER)
    
    ;; Record the event
    (add-product-event product-id event-type details location)
    
    (ok true)
  )
)

;; Mark a product as sold to consumer (end of supply chain)
(define-public (mark-product-sold (product-id uint) (details (string-utf8 200)) (location (optional (string-ascii 100))))
  (let (
    (product (map-get? products { product-id: product-id }))
  )
    (asserts! (is-some product) ERR-INVALID-PRODUCT)
    (asserts! (is-current-custodian product-id tx-sender) ERR-NOT-PRODUCT-OWNER)
    (asserts! (not (get is-sold (unwrap-panic product))) ERR-PRODUCT-ALREADY-SOLD)
    
    ;; Update product status
    (map-set products
      { product-id: product-id }
      (merge (unwrap-panic product) { status: "sold", is-sold: true })
    )
    
    ;; Record sold event
    (add-product-event product-id "sold" details location)
    
    (ok true)
  )
)

;; Read-only Functions

;; Get product details
(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

;; Get product certification
(define-read-only (get-product-certification (product-id uint) (cert-type (string-ascii 30)))
  (map-get? product-certifications { product-id: product-id, cert-type: cert-type })
)

;; Get number of events for a product
(define-read-only (get-product-event-count (product-id uint))
  (default-to { count: u0 } (map-get? product-event-count { product-id: product-id }))
)

;; Get a specific event from product history
(define-read-only (get-product-event (product-id uint) (event-index uint))
  (map-get? product-history { product-id: product-id, event-index: event-index })
)

;; Check if a handler is authorized
(define-read-only (get-handler-info (handler principal))
  (map-get? authorized-handlers { handler-id: handler })
)

;; Get information about a certification authority
(define-read-only (get-certification-authority (authority principal))
  (map-get? certification-authorities { authority-id: authority })
)

;; Verify current custodian of a product
(define-read-only (verify-custodian (product-id uint) (custodian principal))
  (is-current-custodian product-id custodian)
)