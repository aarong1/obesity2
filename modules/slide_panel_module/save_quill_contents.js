$(document).ready(function () {
  function saveToLocalStorage() {
    console.log("Save to localStorage");
    localStorage.setItem("delta", JSON.stringify(window.quill.getContents()));
  }

  $("#save").on("click", function () {
    saveToLocalStorage();
  });
});