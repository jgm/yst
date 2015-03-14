$(document).ready(function(){
  $('.tree-toggle').click(function () {
    $(this).parent().children('ul.tree').toggle(200);
  });
  $('.tree-toggle').each(function () {
    $(this).parent().children('ul.tree').toggle(0);
  });
});
