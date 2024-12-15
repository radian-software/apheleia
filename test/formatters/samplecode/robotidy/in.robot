Force Tags  tags  tag2
Library  Collections
Resource  important.robot
Library   MyCustomLibrary.py

Test Setup  Setup Keyword


*** test case*
Test1
    [ teardown]  Teardown Keyword
    Keyword
    FOR  ${var}  IN RANGE  10
    Run Keyword If  ${var}>5  Other Keyword
    END
*** Variables ***
${var}=  2
${bit_longer}  10
${var2}   a
...  b

*** Keywords ***
