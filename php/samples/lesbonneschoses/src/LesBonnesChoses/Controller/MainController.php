<?php

namespace LesBonnesChoses\Controller;

include "../../../src/wroom.php";

use Symfony\Bundle\FrameworkBundle\Controller\Controller;

class MainController extends Controller {

    private $api = null;

    private function getApi() {
        if (!$this->api) $this->api = new \prismic\API("http://lesbonneschoses.wroom.io/api");
        return $this->api;
    }

    private function master() {
        return $this->getApi()->master();
    }

    public static $CATEGORIES = array(
        "Macaron" => "Macarons",
        "Cupcake" => "Cup Cakes",
        "Pie" => "Little Pies"
    );

    public function indexAction() {
        $products = $this->getApi()->forms()->everything->query($this->master());
        return $this->render('LesBonnesChosesBundle:Main:index.html.twig', array(
            'categories' => self::$CATEGORIES,
            'products' => $products
        ));
    }

}
