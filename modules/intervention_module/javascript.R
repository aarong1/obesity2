first_js <- "// Intervention Chart Module JavaScript
// ECharts-based draggable intervention timeline

class InterventionChart {
  constructor(containerId, moduleId, options = {}) {
    this.containerId = containerId;
    this.moduleId = moduleId;
    this.symbolSize = options.symbolSize || 15;
    this.chart = null;
    this.data = options.initialData || this.getDefaultData();
    
    this.init();
  }
  
  getDefaultData() {
    return [
      [2025, 1], [2026, 1], [2027, 1], [2028, 1], [2029, 1],
      [2030, 1], [2031, 1], [2032, 1], [2033, 1], [2034, 1],
      [2035, 1], [2036, 1], [2037, 1], [2038, 1], [2039, 1],
      [2040, 1], [2041, 1], [2042, 1], [2043, 1], [2044, 1], [2045, 1]
    ];
  }
  
  init() {
    const container = document.getElementById(this.containerId);
    if (!container) {
      console.error(`Container ${this.containerId} not found`);
      return;
    }
    
    this.chart = echarts.init(container);
    this.renderChart();
    this.setupEventListeners();
  }
  
  renderChart() {
    const option = {
      grid: {
        left: 30,
        right: 20,
        top: 30,
        bottom: 30,
        containLabel: true
      },
      tooltip: {
        triggerOn: 'none',
        position: (point, params, dom, rect, size) => {
          const x = point[0];
          const y = point[1];
          const boxHeight = size.contentSize[1];
          return [x - 15, y + boxHeight / 2];
        },
        formatter: (params) => {
          const year = params.data[0];
          const yValue = params.data[1];
          const note = yValue !== 1 ? '<br /><b>Intervened</b>' : '';
          return `Year: ${year}<br /> ${yValue.toFixed(2)}${note}`;
        }
      },
      xAxis: {
        type: 'value',
        min: 2024,
        max: 2046,
        axisLine: { show: false },
        axisTick: { show: false },
        axisLabel: {
          show: true,
          formatter: (value) => String(value)
        },
        splitLine: {
          show: true,
          lineStyle: { type: 'dashed', color: '#ccc' }
        }
      },
      yAxis: {
        type: 'value',
        min: 0,
        max: 2,
        axisLine: { show: false },
        axisTick: { show: false },
        axisLabel: { show: false },
        splitLine: {
          show: true,
          lineStyle: { type: 'dashed', color: '#eee' }
        }
      },
      series: [{
        id: 'intervention_series',
        type: 'scatter',
        smooth: true,
        itemStyle: { color: '#4add8c' },
        symbolSize: this.symbolSize,
        data: this.data
      }]
    };
    
    this.chart.setOption(option);
    this.setupDragHandlers();
  }
  
  setupDragHandlers() {
    this.chart.setOption({
      graphic: echarts.util.map(this.data, (item, dataIndex) => {
        return {
          type: 'circle',
          position: this.chart.convertToPixel('grid', item),
          shape: { r: this.symbolSize / 2 },
          invisible: true,
          draggable: true,
          ondrag: echarts.util.curry(this.onPointDragging.bind(this), dataIndex),
          onmousemove: echarts.util.curry(this.showTooltip.bind(this), dataIndex),
          onmouseout: echarts.util.curry(this.hideTooltip.bind(this), dataIndex),
          z: 100
        };
      })
    });
  }
  
  showTooltip(dataIndex) {
    this.chart.dispatchAction({ type: 'showTip', seriesIndex: 0, dataIndex: dataIndex });
  }
  
  hideTooltip(dataIndex) {
    this.chart.dispatchAction({ type: 'hideTip' });
  }
  
  onPointDragging(dataIndex) {
    const draggedX = this.data[dataIndex][0];
    const newY = this.chart.convertFromPixel('grid', this.position)[1];
    
    // Update y for current point
    if (this.data[dataIndex][0] == draggedX) {
      this.data[dataIndex][1] = newY;
    }
    
    // Get input values using module namespace
    const changeSubsequent = this.getInputValue('change_subsequent_graph');
    const taperSubsequent = this.getInputValue('taper_subsequent_graph');
    
    // Handle subsequent points based on settings
    if (changeSubsequent && taperSubsequent) {
      for (let i = dataIndex; i < this.data.length; i++) {
        if (this.data[i][0] > draggedX) {
          this.data[i][1] = (newY - 1) * ((2046 - this.data[i][0]) / (2046 - draggedX)) + 1;
        }
      }
    } else if (changeSubsequent && !taperSubsequent) {
      for (let i = dataIndex; i < this.data.length; i++) {
        if (this.data[i][0] > draggedX) {
          this.data[i][1] = newY;
        }
      }
    }
    
    this.updateChart();
    this.notifyShiny(dataIndex);
  }
  
  updateChart() {
    this.chart.setOption({
      series: [{ id: 'intervention_series', data: this.data }]
    });
    
    this.chart.setOption({
      graphic: echarts.util.map(this.data, (item, dataIndex) => {
        return {
          position: this.chart.convertToPixel('grid', item)
        };
      })
    });
  }
  
  toggleLineSeries(show) {
    if (show) {
      this.chart.setOption({
        series: [{
          id: 'line_series',
          type: 'line',
          smooth: true,
          lineStyle: { color: '#4add8c', width: 2 },
          symbol: 'none',
          data: this.data
        }]
      });
    } else {
      this.chart.setOption({
        series: [{ id: 'line_series', data: [] }]
      });
    }
  }
  
  getInputValue(inputName) {
    const fullInputName = `${this.moduleId}-${inputName}`;
    return window.Shiny && window.Shiny.shinyapp && 
           window.Shiny.shinyapp.$inputValues ? 
           window.Shiny.shinyapp.$inputValues[fullInputName] : false;
  }
  
  notifyShiny(dataIndex) {
    if (window.Shiny) {
      window.Shiny.setInputValue(`${this.moduleId}-draggable_data`, {
        dataIndex: dataIndex,
        newData: this.data,
        nonce: Math.random()
      });
    }
  }
  
  resize() {
    if (this.chart) {
      this.chart.resize();
      this.chart.setOption({
        graphic: echarts.util.map(this.data, (item, dataIndex) => {
          return { position: this.chart.convertToPixel('grid', item) };
        })
      });
    }
  }
  
  updateData(newData) {
    this.data = newData;
    this.updateChart();
    this.setupDragHandlers();
  }
}

// Global chart instances for module management
window.interventionCharts = window.interventionCharts || {};

// Initialize chart function called from Shiny
window.initInterventionChart = function(containerId, moduleId, options) {
  const chart = new InterventionChart(containerId, moduleId, options);
  window.interventionCharts[moduleId] = chart;
  return chart;
};

// Setup custom message handlers for Shiny integration
if (window.Shiny) {
  // Handle line series toggle
  window.Shiny.addCustomMessageHandler('interventionToggleLine', function(message) {
    const chart = window.interventionCharts[message.moduleId];
    if (chart) {
      chart.toggleLineSeries(message.show);
    }
  });
  
  // Handle data updates
  window.Shiny.addCustomMessageHandler('interventionUpdateData', function(message) {
    const chart = window.interventionCharts[message.moduleId];
    if (chart) {
      chart.updateData(message.data);
    }
  });
}"

second_js <- '// Canvas Dragging Module JavaScript
// Handles drag-and-drop functionality for canvas elements

class CanvasDragManager {
  constructor(moduleId, options = {}) {
    this.moduleId = moduleId;
    this.editorId = options.editorId || "editor";
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
    
    $(document).on("click", `#${toggleCloseId}`, () => {
      this.disableDragging();
    });
    
    $(document).on("click", `#${toggleOpenId}`, () => {
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
      console.warn(`Drop zone element "${this.editorId}" not found`);
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
  window.Shiny.addCustomMessageHandler("canvasDragEnable", function(message) {
    const manager = window.canvasDragManagers[message.moduleId];
    if (manager) {
      manager.setEnabled(message.enabled);
    }
  });
}'