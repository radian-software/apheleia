*** Comments ***
Force Tags    tags    tag2
Library    Collections
Resource    important.robot
Library    MyCustomLibrary.py

Test Setup    Setup Keyword


*** Variables ***
${var}=             2
${bit_longer}       10
${var2}             a
...                 b


*** Test Cases ***
Test1
    Keyword
    FOR    ${var}    IN RANGE    10
        IF    ${var}>5    Other Keyword
    END
    [Teardown]    Teardown Keyword

