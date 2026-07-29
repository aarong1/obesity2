// Canvas Dragging Module JavaScript
// Handles drag-and-drop functionality for canvas elements

class CanvasDragManager {
  constructor(moduleId, options = {}) {
    this.moduleId = moduleId;
    this.editorId = options.editorId || 'editor';
    this.enabled = false;
    
    this.init();
  }
  
  init() {
    this.setupEventListeners();
  }
  
  setupEventListeners() {
    // Listen for enable/disable events from Shiny
    const toggleOpenId = `${this.moduleId}-toggle_open`;
    const toggleCloseId = `${this.moduleId}-toggle_close`;
    
    $(document).on('click', `#${toggleCloseId}`, () => {
      this.disableDragging();
    });
    
    $(document).on('click', `#${toggleOpenId}`, () => {
      this.enableDragging();
    });
  }
  
  enableDragging() {
    this.enabled = true;
    
    // Make canvas and table elements draggable
    $("canvas, table").attr("draggable", true);
    
    // Setup each canvas element
    const canvases = document.getElementsByTagName("canvas");
    Array.from(canvases).forEach(canvas => {
      this.setupCanvas(canvas);
    });
    
    // Setup drop zone
    this.setupDropZone();
    
    console.log(`Canvas dragging enabled for module: ${this.moduleId}`);
  }
  
  disableDragging() {
    this.enabled = false;
    
    // Remove draggable attribute
    $("canvas, table").attr("draggable", false);
    
    console.log(`Canvas dragging disabled for module: ${this.moduleId}`);
  }
  
  setupCanvas(canvas) {
    // Remove existing listeners to prevent duplicates
    canvas.removeEventListener("dragstart", this.handleDragStart);
    
    // Add dragstart event
    canvas.addEventListener("dragstart", this.handleDragStart.bind(this));
  }
  
  handleDragStart(event) {
    console.log("Canvas drag started");
    
    try {
      const canvas = event.target;
      const dataUrl = canvas.toDataURL("image/png");
      
      event.dataTransfer.setData("text/plain", dataUrl);
      console.log("Canvas data URL set for transfer");
      
      // Notify Shiny about drag start
      if (window.Shiny) {
        window.Shiny.setInputValue(`${this.moduleId}-canvas_drag_start`, {
          timestamp: Date.now(),
          hasData: !!dataUrl
        });
      }
    } catch (error) {
      console.error("Error in canvas drag start:", error);
    }
  }
  
  setupDropZone() {
    const editorElement = document.getElementById(this.editorId);
    
    if (!editorElement) {
      console.warn(`Drop zone element '${this.editorId}' not found`);
      return;
    }
    
    // Remove existing listeners
    editorElement.removeEventListener("dragover", this.handleDragOver);
    editorElement.removeEventListener("drop", this.handleDrop.bind(this));
    
    // Add drag over event to allow dropping
    editorElement.addEventListener("dragover", this.handleDragOver);
    
    // Add drop event to insert image
    editorElement.addEventListener("drop", this.handleDrop.bind(this));
  }
  
  handleDragOver(event) {
    event.preventDefault();
  }
  
  handleDrop(event) {
    event.preventDefault();
    
    try {
      const dataUrl = event.dataTransfer.getData("text/plain");
      console.log("Canvas dropped, data URL received");
      
      if (!dataUrl) {
        console.warn("No data URL found in drop event");
        return;
      }
      
      // Handle Quill editor integration if available
      if (window.quill) {
        const range = window.quill.getSelection();
        const index = range ? range.index : 0;
        window.quill.insertEmbed(index, "image", dataUrl);
        console.log("Image inserted into Quill editor");
      }
      
      // Notify Shiny about successful drop
      if (window.Shiny) {
        window.Shiny.setInputValue(`${this.moduleId}-canvas_dropped`, {
          timestamp: Date.now(),
          dataUrl: dataUrl.substring(0, 100) + "..." // Truncated for logging
        });
      }
      
    } catch (error) {
      console.error("Error in canvas drop:", error);
    }
  }
  
  // Public method to programmatically enable/disable
  setEnabled(enabled) {
    if (enabled) {
      this.enableDragging();
    } else {
      this.disableDragging();
    }
  }
}

// Global drag manager instances
window.canvasDragManagers = window.canvasDragManagers || {};

// Initialize drag manager function called from Shiny
window.initCanvasDragManager = function(moduleId, options) {
  const manager = new CanvasDragManager(moduleId, options);
  window.canvasDragManagers[moduleId] = manager;
  return manager;
};

// Setup custom message handlers for Shiny integration
if (window.Shiny) {
  // Handle enable/disable drag commands
  window.Shiny.addCustomMessageHandler('canvasDragEnable', function(message) {
    const manager = window.canvasDragManagers[message.moduleId];
    if (manager) {
      manager.setEnabled(message.enabled);
    }
  });
}