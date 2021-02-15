<?php
/**
 * Created by PhpStorm.
 * User: bunyaminakcay
 * Project name busbud-challenge-1
 * 13.02.2021 20:40
 * Bünyamin AKÇAY <bunyamin@bunyam.in>
 */

use Laravel\Lumen\Routing\UrlGenerator;

if (!function_exists('urlGenerator')) {
    /**
     * @return UrlGenerator
     */
    function urlGenerator() {
        return new UrlGenerator(app());
    }
}

if (!function_exists('asset')) {
    /**
     * @param $path
     * @param bool $secured
     *
     * @return string
     */
    function asset($path, $secured = false) {

        return urlGenerator()->asset($path, $secured);

    }
}
