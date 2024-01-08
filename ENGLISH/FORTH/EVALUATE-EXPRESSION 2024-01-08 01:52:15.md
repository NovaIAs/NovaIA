```forth
: EVALUATE-EXPRESSION
  [s" ]
  LOOP
    SWAP
    2SWAP
    DUP
    TYPE
    IF
      % "]" CHAR=
    ELSE
      [s" ]
      LOOP
        SWAP
        2SWAP
        DUP
        TYPE
        IF
          % "]" CHAR=
        ELSE
          SWAP
          2DROP
          [s" ]
          LOOP
            SWAP
            2SWAP
            DUP
            TYPE
            IF
              % "]" CHAR=
            ELSE
              SWAP
              2DROP
              [s" ]
              LOOP
                SWAP
                2SWAP
                DUP
                TYPE
                IF
                  % "]" CHAR=
                ELSE
                  SWAP
                  2DROP
                  [s" ]
                  LOOP
                    SWAP
                    2SWAP
                    DUP
                    TYPE
                    IF
                      % "]" CHAR=
                    ELSE
                      SWAP
                      2DROP
                      [s" ]
                      LOOP
                        SWAP
                        2SWAP
                        DUP
                        TYPE
                        IF
                          % "]" CHAR=
                        ELSE
                          SWAP
                          2DROP
                          [s" ]
                          LOOP
                            SWAP
                            2SWAP
                            DUP
                            TYPE
                            IF
                              % "]" CHAR=
                            ELSE
                              SWAP
                              2DROP
                              [s" ]
                              LOOP
                                SWAP
                                2SWAP
                                DUP
                                TYPE
                                IF
                                  % "]" CHAR=
                                ELSE
                                  SWAP
                                  2DROP
                                  [s" ]
                                  LOOP
                                    SWAP
                                    2SWAP
                                    DUP
                                    TYPE
                                    IF
                                      % "]" CHAR=
                                    ELSE
                                      SWAP
                                      2DROP
                                      [s" ]
                                      LOOP
                                        SWAP
                                        2SWAP
                                        DUP
                                        TYPE
                                        IF
                                          % "]" CHAR=
                                        ELSE
                                          SWAP
                                          2DROP
                                          [s" ]
                                          LOOP
                                            SWAP
                                            2SWAP
                                            DUP
                                            TYPE
                                            IF
                                              % "]" CHAR=
                                            ELSE
                                              SWAP
                                              2DROP
                                              [s" ]
                                              LOOP
                                                SWAP
                                                2SWAP
                                                DUP
                                                TYPE
                                                IF
                                                  % "]" CHAR=
                                                ELSE
                                                  SWAP
                                                  2DROP
                                                  [s" ]
                                                  LOOP
                                                    SWAP
                                                    2SWAP
                                                    DUP
                                                    TYPE
                                                    IF
                                                      % "]" CHAR=
                                                    ELSE
                                                      SWAP
                                                      2DROP
                                                      [s" ]
                                                      LOOP
                                                        SWAP
                                                        2SWAP
                                                        DUP
                                                        TYPE
                                                        IF
                                                          % "]" CHAR=
                                                        ELSE
                                                          SWAP
                                                          2DROP
                                                          [s" ]
                                                          LOOP
                                                            SWAP
                                                            2SWAP
                                                            DUP
                                                            TYPE
                                                            IF
                                                              % "]" CHAR=
                                                            ELSE
                                                              SWAP
                                                              2DROP
                                                              [s" ]
                                                              LOOP
                                                                SWAP
                                                                2SWAP
                                                                DUP
                                                                TYPE
                                                                IF
                                                                  % "]" CHAR=
                                                                ELSE
                                                                  SWAP
                                                                  2DROP
                                                                  [s" ]
                                                                  LOOP
                                                                    SWAP
                                                                    2SWAP
                                                                    DUP
                                                                    TYPE
                                                                    IF
                                                                      % "]" CHAR=
                                                                    ELSE
                                                                      SWAP
                                                                      2DROP
                                                                      [s" ]
                                                                      LOOP
                                                                        SWAP
                                                                        2SWAP
                                                                        DUP
                                                                        TYPE
                                                                        IF
                                                                          % "]" CHAR=
                                                                        ELSE
                                                                          SWAP
                                                                          2DROP
                                                                          [s" ]
                                                                          LOOP
                                                                            SWAP
                                                                            2SWAP
                                                                            DUP
                                                                            TYPE
                                                                            IF
                                                                              % "]" CHAR=
                                                                            ELSE
                                                                              SWAP
                                                                              2DROP
                                                                              [s" ]
                                                                              LOOP
                                                                                SWAP
                                                                                2SWAP
                                                                                DUP
                                                                                TYPE
                                                                                IF
                                                                                  % "]" CHAR=
                                                                                ELSE
                                                                                  SWAP
                                                                                  2DROP
                                                                                  [s" ]
                                                                                  LOOP
                                                                                    SWAP
                                                                                    2SWAP
                                                                                    DUP
                                                                                    TYPE
                                                                                    IF
                                                                                      % "]" CHAR=
                                                                                    ELSE
                                                                                      SWAP
                                                                                      2DROP
                                                                                      [s" ]
                                                                                      LOOP
                                                                                        SWAP
                                                                                        2SWAP
                                                                                        DUP
                                                                                        TYPE
                                                                                        IF
                                                                                          % "]" CHAR=
                                                                                        ELSE
                                                                                          SWAP
                                                                                          2DROP
                                                                                          [s" ]
                                                                                          LOOP
                                                                                            SWAP
                                                                                            2SWAP
                                                                                            DUP
                                                                                            TYPE
                                                                                            IF
                                                                                              % "]" CHAR=
                                                                                            ELSE
                                                                                              SWAP
                                                                                              2DROP
                                                                                              [s" ]
                                                                                              LOOP
                                                                                                SWAP
                                                                                                2SWAP
                                                                                                DUP
                                                                                                TYPE
                                                                                                IF
                                                                                                  % "]" CHAR=
                                                                                                ELSE
                                                                                                  SWAP
                                                                                                  2DROP
                                                                                                  [s" ]
                                                                                                  LOOP
                                                                                                    SWAP
                                                                                                    2SWAP
                                                                                                    DUP
                                                                                                    TYPE
                                                                                                    IF
                                                                                                      % "]" CHAR=
                                                                                                    ELSE
                                                                                                      SWAP
                                                                                                      2DROP
                                                                                                      [s" ]
                                                                                                      LOOP
                                                                                                        SWAP
                                                                                                        2SWAP
                                                                                                        DUP
                                                                                                        TYPE
                                                                                                        IF
                                                                                                          % "]" CHAR=
                                                                                                        ELSE
                                                                                                          SWAP
                                                                                                          2DROP
                                                                                                          [s" ]
                                                                                                          LOOP
                                                                                                            SWAP
                                                                                                            2SWAP
                                                                                                            DUP
                                                                                                            TYPE
                                                                                                            IF
                                                                                                              % "]" CHAR=
                                                                                                            ELSE
                                                                                                              SWAP
                                                                                                              2DROP
                                                                                                              [s" ]
                                                                                                              LOOP
                                                                                                                SWAP
                                                                                                                2SWAP
                                                                                                                DUP
                                                                                                                TYPE
                                                                                                                IF
                                                                                                                  % "]" CHAR=
                                                                                                                ELSE
                                                                                                                  SWAP
                                                                                                                  2DROP
                                                                                                                  [s" ]
                                                                                                                  LOOP
                                                                                                                    SWAP
                                                                                                                    2SWAP
                                                                                                                    DUP
                                                                                                                    TYPE
                                                                                                                    IF
                                                                                                                      % "]" CHAR=
                                                                                                                    ELSE
                                                                                                                      SWAP
                                                                                                                      2DROP
                                                                                                                      [s" ]
                                                                                                                      LOOP
                                                                                                                        SWAP
                                                                                                                        2SWAP
                                                                                                                        DUP
                                                                                                                        TYPE
                                                                                                                        IF
                                                                                                                          % "]" CHAR=
                                                                                                                        ELSE
                                                                                                                          SWAP
                                                                                                                          2DROP
                                                                                                                          [s" ]
                                                                                                                          LOOP
                                                                                                                            SWAP
                                                                                                                            2SWAP
                                                                                                                            DUP
                                                                                                                            TYPE
                                                                                                                            IF
                                                                                                                              % "]" CHAR=
                                                                                                                            ELSE
                                                                                                                              SWAP
                                                                                                                              2DROP
                                                                                                                              [s" ]
                                                                                                                              LOOP
                                                                                                                                SWAP
                                                                                                                                2SWAP
                                                                                                                                DUP
                                                                                                                                TYPE
                                                                                                                                IF
                                                                                                                                  % "]" CHAR=
                                                                                                                                ELSE
                                                                                                                                  SWAP
                                                                                                                                  2DROP
                                                                                                                                  [s" ]
                                                                                                                                  LOOP
                                                                                                                                    SWAP
                                                                                                                                    2SWAP
                                                                                                                                    DUP
                                                                                                                                    TYPE
                                                                                                                                    IF
                                                                                                                                      % "]" CHAR=
                                                                                                                                    ELSE
                                                                                                                                      SWAP
                                                                                                                                      2DROP
                                                                                                                                      [s" ]
                                                                                                                                      LOOP
                                                                                                                                        SWAP
                                                                                                                                        2SWAP
                                                                                                                                        DUP
                                                                                                                                        TYPE
                                                                                             