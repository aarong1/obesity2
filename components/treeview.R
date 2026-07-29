treeview <- function() {
  #https://www.w3schools.com/howto/howto_js_treeview.asp
  div(
    class = 'treeeview',
    includeScript(path = './www/js/treeview.js'),
    
    HTML('<link  rel="stylesheet" href = css/treeview.css />'),
    HTML(
      '
<ul id="myUL">
  <li><span class="caret">Beverages</span>
    <ul class="nested">
      <li>modifiable</li>
      <li>non-modifiable</li>
      <li><span class="caret">Tea</span>
        <ul class="nested">
          <li>Age</li>
          <li>Gender</li>
          <li><span class="caret">Green Tea</span>
            <ul class="nested">
              <li>Sencha</li>
              <li>Gyokuro</li>
              <li>Matcha</li>
              <li>Pi Lo Chun</li>
            </ul>
          </li>
        </ul>
      </li>
    </ul>
  </li>
</ul>

  '
    )
  )
}
