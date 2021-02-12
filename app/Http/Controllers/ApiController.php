<?php namespace App\Http\Controllers;

use App\Http\Responses\Output;

use Illuminate\Http\Response;

use UnexpectedValueException;

class ApiController extends Controller {

    /**
     * An output class for sending expected outputs.
     * 
     * @var App\Http\Responses\Output
     */
    private $output;

    /**
     * Current status code of the given request.
     * 
     * @var integer
     */
    protected $statusCode = Response::HTTP_OK;

    /**
     * Make a new api controller with an output class.
     * 
     * @param App\Http\Responses\Output $output
     */
    public function __construct(Output $output) 
    {
        $this->output = $output;
    }

    /**
     * Get the current status code.
     * 
     * @return integer
     */
    public function getStatusCode()
    {
        return $this->statusCode;
    }

    /**
     * Respond with a json array.
     * 
     * @param  array  $array
     * @param  array  $headers
     * @return Illuminate\Http\Response
     */
    protected function respondWithArray(array $array, array $headers = array())
    {
        return response()->json($array, $this->statusCode, $headers);
    }

    /**
     * Respond with an error.
     * 
     * @param  stirng $message  
     * @param  stirng $errorCode
     * @return Illuminate\Http\Response
     */
    protected function respondWithError($message, $errorCode)
    {
        if ($this->statusCode == Response::HTTP_OK) {
            throw new UnexpectedValueException('Error response requested with 200 status code; user error.');
        }

        $out = $this->output->asError($message, $this->statusCode, $errorCode);

        return $this->respondWithArray($out);
    }

    /**
     * Respond with a single item.
     * 
     * @param  mixed $item    
     * @param  mixed $callback
     * @return Illuminate\Http\Response
     */
    protected function respondWithItem($item, $callback)
    {
        $out = $this->output->asItemArray($item, $callback);

        return $this->respondWithArray($out);
    }

    /**
     * Respond with a collection of items.
     * 
     * @param  array $collection
     * @param  mixed $callback  
     * @return Illuminate\Http\Response 
     */
    protected function respondWithCollection($collection, $callback)
    {
        $out = $this->output->asCollectionArray($collection, $callback);

        return $this->respondWithArray($out);
    }
}