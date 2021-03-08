<?php

namespace App\Entity;

use App\Repository\CityRepository;
use Doctrine\ORM\Mapping as ORM;

/**
 * @ORM\Entity(repositoryClass=CityRepository::class)
 */
class City
{
    /**
     * @ORM\Id
     * @ORM\GeneratedValue
     * @ORM\Column(type="integer")
     */
    private $id;

    /**
     * @ORM\Column(type="string", length=200)
     */
    private $name;

    /**
     * @ORM\Column(type="string", length=200)
     */
    private $asciiName;

    /**
     * @ORM\Column(type="text", nullable=true)
     */
    private $alternateNames;

    /**
     * @ORM\Column(type="float")
     */
    private $latitude;

    /**
     * @ORM\Column(type="float")
     */
    private $longitude;

    /**
     * @ORM\Column(type="string", length=1)
     */
    private $featureClass;

    /**
     * @ORM\Column(type="string", length=10)
     */
    private $featureCode;

    /**
     * @ORM\Column(type="string", length=2)
     */
    private $countryCode;

    /**
     * @ORM\Column(type="string", length=60, nullable=true)
     */
    private $cc2;

    /**
     * @ORM\Column(type="string", length=20, nullable=true)
     */
    private $admin1Code;

    /**
     * @ORM\Column(type="string", length=20, nullable=true)
     */
    private $admin2Code;

    /**
     * @ORM\Column(type="string", length=20, nullable=true)
     */
    private $admin3Code;

    /**
     * @ORM\Column(type="string", length=20, nullable=true)
     */
    private $admin4Code;

    /**
     * @ORM\Column(type="bigint")
     */
    private $population;

    /**
     * @ORM\Column(type="integer", nullable=true)
     */
    private $elevation;

    /**
     * @ORM\Column(type="integer", nullable=true)
     */
    private $dem;

    /**
     * @ORM\Column(type="string", length=40, nullable=true)
     */
    private $timeZone;

    /**
     * @ORM\Column(type="date")
     */
    private $modificationDate;

    public function getId(): ?int
    {
        return $this->id;
    }

    public function getName(): ?string
    {
        return $this->name;
    }

    public function setName(string $name): self
    {
        $this->name = $name;

        return $this;
    }

    public function getAsciiName(): ?string
    {
        return $this->asciiName;
    }

    public function setAsciiName(string $asciiName): self
    {
        $this->asciiName = $asciiName;

        return $this;
    }

    public function getAlternateNames(): ?string
    {
        return $this->alternateNames;
    }

    public function setAlternateNames(?string $alternateNames): self
    {
        $this->alternateNames = $alternateNames;

        return $this;
    }

    public function getLatitude(): ?float
    {
        return $this->latitude;
    }

    public function setLatitude(float $latitude): self
    {
        $this->latitude = $latitude;

        return $this;
    }

    public function getLongitude(): ?float
    {
        return $this->longitude;
    }

    public function setLongitude(float $longitude): self
    {
        $this->longitude = $longitude;

        return $this;
    }

    public function getFeatureClass(): ?string
    {
        return $this->featureClass;
    }

    public function setFeatureClass(string $featureClass): self
    {
        $this->featureClass = $featureClass;

        return $this;
    }

    public function getFeatureCode(): ?string
    {
        return $this->featureCode;
    }

    public function setFeatureCode(string $featureCode): self
    {
        $this->featureCode = $featureCode;

        return $this;
    }

    public function getCountryCode(): ?string
    {
        return $this->countryCode;
    }

    public function setCountryCode(string $countryCode): self
    {
        $this->countryCode = $countryCode;

        return $this;
    }

    public function getCc2(): ?string
    {
        return $this->cc2;
    }

    public function setCc2(?string $cc2): self
    {
        $this->cc2 = $cc2;

        return $this;
    }

    public function getAdmin1Code(): ?string
    {
        return $this->admin1Code;
    }

    public function setAdmin1Code(?string $admin1Code): self
    {
        $this->admin1Code = $admin1Code;

        return $this;
    }

    public function getAdmin2Code(): ?string
    {
        return $this->admin2Code;
    }

    public function setAdmin2Code(?string $admin2Code): self
    {
        $this->admin2Code = $admin2Code;

        return $this;
    }

    public function getAdmin3Code(): ?string
    {
        return $this->admin3Code;
    }

    public function setAdmin3Code(?string $admin3Code): self
    {
        $this->admin3Code = $admin3Code;

        return $this;
    }

    public function getAdmin4Code(): ?string
    {
        return $this->admin4Code;
    }

    public function setAdmin4Code(?string $admin4Code): self
    {
        $this->admin4Code = $admin4Code;

        return $this;
    }

    public function getPopulation(): ?string
    {
        return $this->population;
    }

    public function setPopulation(string $population): self
    {
        $this->population = $population;

        return $this;
    }

    public function getElevation(): ?int
    {
        return $this->elevation;
    }

    public function setElevation(?int $elevation): self
    {
        $this->elevation = $elevation;

        return $this;
    }

    public function getDem(): ?int
    {
        return $this->dem;
    }

    public function setDem(?int $dem): self
    {
        $this->dem = $dem;

        return $this;
    }

    public function getTimeZone(): ?string
    {
        return $this->timeZone;
    }

    public function setTimeZone(?string $timeZone): self
    {
        $this->timeZone = $timeZone;

        return $this;
    }

    public function getModificationDate(): ?\DateTimeInterface
    {
        return $this->modificationDate;
    }

    public function setModificationDate(\DateTimeInterface $modificationDate): self
    {
        $this->modificationDate = $modificationDate;

        return $this;
    }
}
