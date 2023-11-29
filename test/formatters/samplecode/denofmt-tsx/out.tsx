const Foo: FooComponent = ({
  name: string,
  test: number,
}) => {
  return (
    <>
      <h1>hello, {name}</h1>
      <div>{test}</div>
    </>
  );
};

const Bar: BarComponent = () => {
  return (
    <>
      <Foo name="aaa" test={1} />
    </>
  );
};
