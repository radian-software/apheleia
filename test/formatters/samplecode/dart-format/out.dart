void main() {
  Function addNumbers = (a, b) => print(a + b);
  someOtherFunction("addNumbers", addNumbers);

  var myFunc = taskToPerform();
  print(myFunc(10));
}

void someOtherFunction(String message, Function myFunction) {
  print(message);
  myFunction(2, 4);
}

Function taskToPerform() {
  Function multiplyFour = (int number) => number * 4;
  return multiplyFour;
}
