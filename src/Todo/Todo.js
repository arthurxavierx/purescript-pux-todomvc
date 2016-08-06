// module Todo.Todo

exports.focusTodo = function(id) {
  return function() {
    var $el = document.getElementById('todo-' + id);
    if (document.activeNode !== $el) {
      $el.focus();
    }
  };
};
