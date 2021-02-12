<?php namespace App\Exceptions;

use Exception;
use Illuminate\Foundation\Exceptions\Handler as ExceptionHandler;

class Handler extends ExceptionHandler {

	protected $dontReport = [
		'Symfony\Component\HttpKernel\Exception\HttpException'
	];

	public function report(Exception $e)
	{
		return parent::report($e);
	}

	public function render($request, Exception $e)
	{
		if ($this->isHttpException($e))
		{
			return $this->renderHttpException($e);
		}
		else
		{
			return parent::render($request, $e);
		}
	}

}
