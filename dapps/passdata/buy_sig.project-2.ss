#|
To generate from the command-line in the glow directory:
./compiler/project/t/project-2-test.ss
To run from gxi in the glow directory, assuming gerbil-etherum is in a sibling directory:
> (add-load-path (path-normalize "../gerbil-ethereum"))
> (import "dapps/buy_sig.project-2.ss" :mukn/ethereum/testing :clan/persist/content-addressing)
> ((payForSignature alice-address bob-address) (digest<-string "hello") 1)
|#
(export #t)
(import :mukn/glow/compiler/project/runtime-2)
(def payForSignature-consensus
     (lambda (in out Buyer Seller)
       (lambda (digest0 price)
         (parameterize ((current-input-channel in) (current-output-channel out))
           (begin0 (let ()
                     (consensus:set-participant Buyer)
                     (expect-deposited price)
                     (consensus:set-participant Seller)
                     (consensus:set-participant Seller)
                     (def signature (expect-published 'signature Signature))
                     (def tmp (%%app isValidSignature Seller digest0 signature))
                     (assert! tmp)
                     (consensus:withdraw Seller price)
                     (vector))
                   (consensus:end-interaction))))))
(def payForSignature-Buyer
     (lambda (in0 out0 Buyer Seller)
       (lambda (digest0 price)
         (parameterize ((current-input-channel in0) (current-output-channel out0))
           (begin0 (let ()
                     (participant:set-participant Buyer)
                     (add-to-deposit price)
                     (participant:set-participant Seller)
                     (participant:set-participant Seller)
                     (def signature (expect-published 'signature Signature))
                     (def tmp (%%app isValidSignature Seller digest0 signature))
                     (assert! tmp)
                     (participant:withdraw Seller price)
                     (vector))
                   (participant:end-interaction))))))
(def payForSignature-Seller
     (lambda (in1 out1 Buyer Seller)
       (lambda (digest0 price)
         (parameterize ((current-input-channel in1) (current-output-channel out1))
           (begin0 (let ()
                     (participant:set-participant Buyer)
                     (expect-deposited price)
                     (participant:set-participant Seller)
                     (def signature (sign digest0))
                     (participant:set-participant Seller)
                     (add-to-publish 'signature signature Signature)
                     (def tmp (%%app isValidSignature Seller digest0 signature))
                     (assert! tmp)
                     (participant:withdraw Seller price)
                     (vector))
                   (participant:end-interaction))))))
(def ((payForSignature Buyer Seller) digest0 price)
     (def consensus->Buyer (make-channel #f))
     (def consensus->Seller (make-channel #f))
     (def consensus->participants (@list consensus->Buyer consensus->Seller))
     (def participant->consensus (make-channel #f))
     (def balances (get-balances (@list Buyer Seller)))
     (def consensus-thread
          (spawn/name/params
           'consensus
           (lambda ()
             (parameterize ((current-address #f) (current-balances balances))
               ((payForSignature-consensus
                 participant->consensus
                 consensus->participants
                 Buyer
                 Seller)
                digest0
                price)))))
     (def participant-threads
          (@list (spawn/name/params
                  'Buyer
                  (lambda ()
                    (parameterize ((current-address Buyer) (current-balances balances))
                      ((payForSignature-Buyer
                        consensus->Buyer
                        participant->consensus
                        Buyer
                        Seller)
                       digest0
                       price))))
                 (spawn/name/params
                  'Seller
                  (lambda ()
                    (parameterize ((current-address Seller) (current-balances balances))
                      ((payForSignature-Seller
                        consensus->Seller
                        participant->consensus
                        Buyer
                        Seller)
                       digest0
                       price))))))
     (for-each thread-join! (cons consensus-thread participant-threads))
     (channel-close consensus->Buyer)
     (channel-close consensus->Seller)
     (channel-close participant->consensus)
     'done)
(vector)
