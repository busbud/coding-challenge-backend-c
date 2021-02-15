<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
    <title>Busbud Challenge</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">

      @php($_secure = (isset($_SERVER['HTTP_X_FORWARDED_PROTO']) && $_SERVER['HTTP_X_FORWARDED_PROTO']=='https' ) )

    <link rel="stylesheet" href="{{ asset('/assets/css/application.css',$_secure)}}?v=1">
  </head>
  <body>
    <div class="container">
      <h1 class="title">Location Suggestion</h1>

      <div id="location">
        <h2 class="example-name">Search by name</h2>
        <p >
            Type a location name in United States and Canada<br>
            Results are limited population over 5000 people
        </p>


        <div class="demo">
          <input class="typeahead" type="text" placeholder="Type a place name to search">
            <img src="{{asset('/assets/img/45.gif')}}" id="awloading" style="display:none;position: absolute;top: 10px;margin-left: -41px;">

        </div>

          <p id="permission1" style="display: none">If you accept location permission on your browser. Results will be optimize for you</p>


      </div>

    </div>

    <script type="text/javascript">
        let suggestionpath = '{{route('suggestionpath',[],$_secure)}}';
    </script>

    <script src="{{ asset('/assets/js/handlebars.js',$_secure)}}"></script>
    <script src="{{ asset('/assets/js/jquery-1.10.2.min.js',$_secure)}}"></script>
    <script src="{{ asset('/assets/js/typeahead.bundle.js',$_secure)}}"></script>
    <script src="{{ asset('/assets/js/examples.js',$_secure)}}"></script>

  </body>
</html>

