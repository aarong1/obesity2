 $(document).ready(function () {

// Initialize Quill editor 

  const quill = new Quill("#main1-editor", {
    theme: "bubble"
  });

  //$( function() {
  //  $( "#slidePanel" ).resizable({
  //    //animate: true,
  //      handles: "w"
  //  });
  //} );
  
  // Store the editor instance globally for later use
  window.quill = quill;
  console.log('window.quill' + window.quill);
  



if (localStorage.getItem("delta") === null) {
  console.log('no local storage found');
  
} else {
    console.log(' local storage under name delta - found! ');

const blob = localStorage.getItem("delta");;
const delta = JSON.parse(localStorage.getItem('delta'));
const content = quill.setContents(delta);
}



//Shiny.addCustomMessageHandler('downloadHTML', function(message) {
//  
//  if (window.editorInstance) {
//    window.editorInstance.save().then((outputData) => {
//      // Convert Editor.js output to HTML
//      console.log('check1');
//      const edjsParser = edjsHTML();
//      const html = edjsParser.parse(outputData);
//      console.log('check2');
//      
//      // Send the HTML back to R Shiny
//      Shiny.setInputValue('html_report', html.join(''));
//    }).catch((error) => {
//      console.error('Saving failed: ', error);
//    });
//  }
//
//});

Shiny.addCustomMessageHandler('downloadHTML', function(message) {
  if (window.editorInstance) {
    window.editorInstance.save().then((outputData) => {
      const edjsParser = edjsHTML({
        image: function(block) {
          const imageUrl = block.data.url;  // Should be a Base64 string
          console.log(JSON.stringify(imageUrl));
          console.log(`<img src="${imageUrl}" alt="alt" >`);
          return `<img src="${imageUrl}" alt="alt" >`;
        },
        header: function(block) {
          return `<h${block.data.level}>${block.data.text}</h${block.data.level}>`;
        },
        list: function(block) {
          const items = block.data.items.map(item => `<li>${item}</li>`).join('');
          return `<ul>${items}</ul>`;
        }
      });
      console.log('output data  : '  +   outputData)
      const html = edjsParser.parse(outputData).join('');

      // Send the HTML back to R Shiny
      Shiny.setInputValue('html_report', html);
    }).catch((error) => {
      console.error('Saving failed: ', error);
    });
  }
});

function tableToJson(table) { 
    var data = [];
    for (var i = 1; i < table.rows.length; i++) { 
        var tableRow = table.rows[i]; 
        var rowData = []; 
        for (var j = 0; j < tableRow.cells.length; j++) { 
          if(tableRow.cells[j]){
            rowData.push(tableRow.cells[j].innerHTML);
          } else{
            rowData.push(null);
          }
        }
        
        data.push(rowData); 
    } 
    return data; 
}

Shiny.addCustomMessageHandler('get_table', function(message) {
  console.log('save_table called js side ok');
  var tbl = document.getElementsByClassName('pvtTable') ;
  console.log( JSON.stringify(tableToJson( tbl[0] )));
  
  Shiny.setInputValue('tbl', tbl[0]);
  
  return JSON.stringify(tbl[0]);
  
});
});

