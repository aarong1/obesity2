$(document).ready(function() {

   document.getElementById('copy_paste').addEventListener('click', async function() {
     
    console.log('in clipboard js function');
    
    try {
      

        //const contentToCopy = document.getElementById('editorjs').outerHTML;
        //console.log(contentToCopy);
        //await navigator.clipboard.writeText(contentToCopy);
        //console.log('after await')
        //alert('Content copied to clipboard!');

         const richContentDiv = document.getElementById('editor');

        // Create a new Blob object with the div content as HTML
        const blob = new Blob([richContentDiv.outerHTML], 
        { type: 'text/html' });

        console.log(blob);

        // Create a ClipboardItem from the Blob
        const clipboardItem = new ClipboardItem({ 'text/html': blob });

         const blob1 = new Blob([richContentDiv.innerHTML], 
        { type: 'text/html' });

        const clipboardItem1 = new ClipboardItem({ 'text/html': blob1 });

        // Write the ClipboardItem to the clipboard
        await navigator.clipboard.write([clipboardItem]);
        await navigator.clipboard.write([clipboardItem1]);
        
        alert('Content copied to clipboard!');

        navigator.clipboard.read().then(function(clipboardItems) {
    for (let item of clipboardItems) {
        if (item.types.includes('text/html')) {
            item.getType('text/html').then(function(blob) {
                blob.text().then(function(htmlContent) {
                    // Send the rich text content to the server
                    Shiny.setInputValue('clipboard_html', htmlContent, {priority: 'event'});
                });
            });
        }
    }
});

    } catch (err) {
        console.error('Failed to copy: ', err);
    }
    
});

// Handle drag-and-drop
document.addEventListener('dragover', function(event) {
    event.preventDefault();
});

document.addEventListener('drop', function(event) {
    event.preventDefault();

    const quillContainer = document.querySelector('#editor');
    const isInsideEditor = event.target.closest('#editor');

    if (isInsideEditor && event.dataTransfer.types.includes('text/html')) {
        const htmlData = event.dataTransfer.getData('text/html');

        const tempElement = document.createElement('div');
        tempElement.innerHTML = htmlData;
        console.log(htmlData);
        const tableElement = tempElement.querySelector('table');

        if (tableElement) {
            quill.clipboard.dangerouslyPasteHTML(quill.getSelection().index, tableElement.outerHTML);
        }
    }
});


   
 });
