// module Todo.Todo

exports.focusTodo = function(id) {
  var $el = document.getElementById('todo-' + id);
  if (document.activeNode !== $el) {
    setTimeout($el.focus.bind($el), 100);
  }
};
