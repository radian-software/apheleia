const Foo = ({ name, test }) => {
  return (
    <>
      <h1>hello, {name}</h1>
      <div>{test}</div>
    </>
  );
};

const Bar = () => {
  return (
    <>
      <Foo name="aaa" test="bbb" />
    </>
  );
};
