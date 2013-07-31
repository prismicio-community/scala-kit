<?php

namespace LesBonnesChoses\Controller;

include "../../../src/wroom.php";

use Symfony\Bundle\FrameworkBundle\Controller\Controller;

class MainController extends Controller {

    private function getApi() {
        return new \prismic\API("http://lesbonneschoses.wroom.io/api");
    }

    private function master() {
        return $this->getApi()->master();
    }

    public static $CATEGORIES = array(
        "Macaron" => "Macarons",
        "Cupcake" => "Cup Cakes",
        "Pie" => "Little Pies"
    );

    private static function filterTag($products, $tag) {
        return array_filter($products, function($product) use(&$tag) {
            return in_array($tag, $product->tags());
        });
    }

    public function indexAction() {
        $products = $this->getApi()->forms()->products->query($this->master());
        $byCategory = array(
            "Macaron" => self::filterTag($products, "Macaron"),
            "Cupcake" => self::filterTag($products, "Cupcake"),
            "Pie" => self::filterTag($products, "Pie")
        );
        return $this->render('LesBonnesChosesBundle:Main:index.html.twig', array(
            'categories' => self::$CATEGORIES,
            'products' => $byCategory
        ));
    }

    public function aboutAction() {
        $aboutId = $this->getApi()->bookmarks()->about;
        $about = $this->getApi()->document($this->master(), $aboutId);
        return $this->render('LesBonnesChosesBundle:Main:about.html.twig', array(
            'about' => $about
        ));
    }

}
