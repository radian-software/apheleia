<script> let attrName = "myattr"; let title = "title"; const myFunc = () => {title = "updated title";}; </script>
<div class="foo">
<ul class={myattr}>
<li><a href="/">Home</a></li>
</ul>
</div>
<style>
  .foo { background: #ababab; }
</style>
