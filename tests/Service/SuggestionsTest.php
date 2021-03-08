<?php

namespace App\Tests\Service;

use App\Service\Suggestions;
use App\Tests\Service\Suggestions\MockBuilder\RequestBuilder;
use PHPUnit\Framework\Assert;
use Symfony\Bundle\FrameworkBundle\Test\KernelTestCase;
use Symfony\Component\OptionsResolver\Exception\MissingOptionsException;

class SuggestionsTest extends KernelTestCase
{
    /** @var Suggestions */
    private static $suggestions;

    /** @var RequestBuilder */
    private static $requestBuilder;

    protected function setUp(): void
    {
        self::bootKernel();
        self::$suggestions = self::$container->get('App\Service\Suggestions');
        self::$requestBuilder = new RequestBuilder();
    }

    public function testGetSuggestionsByRequestForValidRequest()
    {
        $validRequest = self::$requestBuilder->buildValidRequest();
        $suggestionResult = self::$suggestions->getSuggestionsByRequest($validRequest);

        Assert::assertIsArray($suggestionResult);
        Assert::assertNotEmpty($suggestionResult);

        $firstSuggestionResult = current($suggestionResult);
        Assert::assertSame(['name', 'latitude', 'longitude', 'score'], array_keys($firstSuggestionResult));
        Assert::assertSame(1, $firstSuggestionResult['score']);
        Assert::assertStringContainsStringIgnoringCase($validRequest->query->get('q'), $firstSuggestionResult['name']);
        Assert::assertIsString($firstSuggestionResult['name']);
        Assert::assertIsFloat($firstSuggestionResult['latitude']);
        Assert::assertIsFloat($firstSuggestionResult['longitude']);

        $lastSuggestionResult = end($suggestionResult);

        if (count($suggestionResult) > 1) {
            Assert::assertLessThan(1, $lastSuggestionResult['score']);
        }
    }

    public function testGetSuggestionsByRequestForValidRequestWithCoordinate()
    {
        $validRequest = self::$requestBuilder->buildValidRequest();

        $validRequest->query->set('latitude', '44.39276');
        $validRequest->query->set('longitude', '-88.73983');

        $suggestionResult = self::$suggestions->getSuggestionsByRequest($validRequest);

        $firstSuggestionResult = current($suggestionResult);

        Assert::assertSame('New London, WI, US', $firstSuggestionResult['name']);

        $validRequest->query->set('latitude', '38.88645');
        $validRequest->query->set('longitude', '-83.44825');

        $suggestionResult = self::$suggestions->getSuggestionsByRequest($validRequest);

        $firstSuggestionResult = current($suggestionResult);

        Assert::assertSame('London, OH, US', $firstSuggestionResult['name']);
    }

    public function testGetSuggestionsByRequestForInValidRequestMethod()
    {
        $inValidRequest = self::$requestBuilder->buildValidRequestForParameters();

        $suggestionResult = null;

        try {
            $suggestionResult = self::$suggestions->getSuggestionsByRequest($inValidRequest);

            Assert::assertTrue(false);
        } catch (\Throwable $exception) {
            Assert::assertInstanceOf(MissingOptionsException::class, $exception);
        }

        Assert::assertNull($suggestionResult);
    }
}