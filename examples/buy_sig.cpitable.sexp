(hash (begin
        (ci 'begin
            (@list)
            (@list)
            (@list (ti 'begin 'end #f (@list) (@list '(payForSignature . #f)) (@list) (@list)))))
      (begin0 (ci 'begin0
                  (@list)
                  (@list)
                  (@list (ti 'begin0
                             'cp
                             'Buyer
                             (@list (syntax (deposit! Buyer price)))
                             (@list)
                             (@list '(Buyer . #f) '(price . #f))
                             (@list)))))
      (cp (ci 'cp
               (@list)
               (@list (ti 'begin0
                          'cp
                          'Buyer
                          (@list (syntax (deposit! Buyer price)))
                          (@list)
                          (@list '(Buyer . #f) '(price . #f))
                          (@list)))
               (@list (ti 'cp
                          'end0
                          'Seller
                          (@list (syntax (withdraw! Seller price))
                                 (syntax (publish! Seller signature)))
                          (@list '(tmp . #f) '(signature . #f))
                          (@list '(Seller . #f)
                                 '(price . #f)
                                 '(tmp . #f)
                                 '(isValidSignature . #f)
                                 '(Seller . #f)
                                 '(digest0 . #f)
                                 '(signature . #f)
                                 '(signature . Seller)
                                 '(Seller . #f))
                          (@list)))))
      (end (ci 'end
               (@list)
               (@list (ti 'begin 'end #f (@list) (@list '(payForSignature . #f)) (@list) (@list)))
               (@list)))
      (end0 (ci 'end0
                (@list)
                (@list (ti 'cp
                           'end0
                           'Seller
                           (@list (syntax (withdraw! Seller price))
                                  (syntax (publish! Seller signature)))
                           (@list '(tmp . #f) '(signature . #f))
                           (@list '(Seller . #f)
                                  '(price . #f)
                                  '(tmp . #f)
                                  '(isValidSignature . #f)
                                  '(Seller . #f)
                                  '(digest0 . #f)
                                  '(signature . #f)
                                  '(signature . Seller)
                                  '(Seller . #f))
                           (@list)))
                (@list))))
