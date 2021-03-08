<?php

namespace App\Controller;

use App\Service\Suggestions;
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\OptionsResolver\Exception\MissingOptionsException;
use Symfony\Component\Routing\Annotation\Route;

/**
 * @Route("/suggestions")
 */
class SuggestionsController extends AbstractController
{
    /**
     * @Route("/", name="suggestions", methods={"GET"})
     */
    public function index(Request $request, Suggestions $suggestions): Response
    {
        try {
            $suggestions = $suggestions->getSuggestionsByRequest($request);
        } catch (MissingOptionsException $missingOptionsException) {
            return $this->json([
                'error' => true,
                'message' => 'Parameters Missing! Required parameters: q (string), Optional parameters: latitude (float), longitude (float)'
            ]);
        } catch (\Throwable $throwable) {
            return $this->json([
                'error' => true,
                'message' => 'An unexpected error has occurred! Please contact admin.'
            ]);
        }

        return $this->json($suggestions);
    }
}
