const Foo: FooComponent = ({
name,
              test}
            ) => {
  return  <>
            <h1>hello, {name}</h1>
 <div>{test}</div>            
    </>
;
};

const Bar
: BarComponent = ()=> {
  return (
    <>
 <Foo name="aaa"
test="bbb" />
      </>
  )
}
