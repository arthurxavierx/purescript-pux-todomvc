// module Todo.Todo

exports.focusTodo = function(id) {
  if(!id) return;
  var $el = document.getElementById('todo-' + id);
  if(document.activeNode !== $el) {
    $el.focus();
  }
};
