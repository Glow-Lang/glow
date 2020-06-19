(hash (begin
        (ci 'begin
            (@list)
            (@list)
            (@list (ti 'begin 'end #f (@list) (@list 'payForSignature) (@list) (@list)))))
      (begin0 (ci 'begin0
                  (@list)
                  (@list)
                  (@list (ti 'begin0 'cp #f (@list) (@list) (@list) (@list)))))
      (cp (ci 'cp
              (@list)
              (@list (ti 'begin0 'cp #f (@list) (@list) (@list) (@list)))
              (@list (ti 'cp
                         'cp0
                         'Buyer
                         (@list (syntax (deposit! Buyer price)))
                         (@list)
                         (@list 'Buyer 'price)
                         (@list)))))
      (cp0 (ci 'cp0
               (@list)
               (@list (ti 'cp
                          'cp0
                          'Buyer
                          (@list (syntax (deposit! Buyer price)))
                          (@list)
                          (@list 'Buyer 'price)
                          (@list)))
               (@list (ti 'cp0
                          'end0
                          '#t
                          (@list (syntax (withdraw! Seller price))
                                 (syntax (publish! Seller signature)))
                          (@list 'tmp)
                          (@list 'Seller
                                 'price
                                 'tmp
                                 'isValidSignature
                                 'Seller
                                 'digest0
                                 'signature
                                 'Seller
                                 'signature)
                          (@list)))))
      (end (ci 'end
               (@list)
               (@list (ti 'begin 'end #f (@list) (@list 'payForSignature) (@list) (@list)))
               (@list)))
      (end0 (ci 'end0
                (@list)
                (@list (ti 'cp0
                           'end0
                           '#t
                           (@list (syntax (withdraw! Seller price))
                                  (syntax (publish! Seller signature)))
                           (@list 'tmp)
                           (@list 'Seller
                                  'price
                                  'tmp
                                  'isValidSignature
                                  'Seller
                                  'digest0
                                  'signature
                                  'Seller
                                  'signature)
                           (@list)))
                (@list))))
