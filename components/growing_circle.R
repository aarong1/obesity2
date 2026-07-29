HTML("
<head>

  <style>
    body {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh;
      margin: 0;
      background: #f4f4f4;
    }

    .box {
      position: relative;
      width: 200px;
      height: 200px;
      background: #fff;
      overflow: hidden;
    }

    .box::before {
      content: '';
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      border: 4px solid green;
      transform: scale(0);
      transform-origin: center;
      transition: transform 1s ease-in-out;
    }

    .box:hover::before {
      transform: scale(1);
    }
  </style>
</head>
<body>
  <div class='box'></div>
</body>
</html>") %>% htmltools::browsable()
