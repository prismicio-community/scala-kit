<?php

namespace prismic;

if (!function_exists('curl_init')) {
    throw new Exception('Wroom needs the CURL PHP extension.');
}
if (!function_exists('json_decode')) {
    throw new Exception('Wroom needs the JSON PHP extension.');
}

class API {

    protected $url;
    protected $apidata = null;

    /**
     * Default options for curl.
     */
    public static $CURL_OPTS = array(
        CURLOPT_CONNECTTIMEOUT => 10,
        CURLOPT_RETURNTRANSFER => true,
        CURLOPT_TIMEOUT        => 60,
        CURLOPT_USERAGENT      => 'wroom-php-0.1',
        CURLOPT_HTTPHEADER     => array('Accept: application/json')
    );

    /**
     * @param $url full qualified URL of the Wroom server to access
     */
    function __construct($url) {
        $this->url = $url;
    }

    /**
     * @return mixed Array of the references present on the server
     */
    public function refs() {
        $refs = $this->getApiData()->refs;
        foreach($refs as $ref) {
            $refs[$ref->label] = $ref;
        }
        unset($ref);
        return $refs;
    }

    /**
     * @return array of bookmarks present on the server
     */
    public function bookmarks() {
        return $this->getApiData()->bookmarks;
    }

    /**
     * @return the master reference
     */
    public function master() {
        $masters = array_filter($this->getApiData()->refs, function ($ref) { return $ref->isMasterRef == true; });
        return $masters[0];
    }

    public function forms() {
        $forms = $this->getApiData()->forms;
        foreach($forms as $key => $form) {
            $forms->$key = new SearchForm($this, $form);
        }
        return $forms;
    }

    /**
     * Return apidata, but fetch lazily (to avoid fetching the apidata if no call is done)
     */
    public function getApiData() {
        if (!$this->apidata) {
            $this->apidata = json_decode(self::get($this->url));
        }
        return $this->apidata;
    }

    public static function get($url) {
        $ch = curl_init();

        $opts = self::$CURL_OPTS;
        $opts[CURLOPT_URL] = $url;

        // disable the 'Expect: 100-continue' behaviour. This causes CURL to wait
        // for 2 seconds if the server does not support this header.
        if (isset($opts[CURLOPT_HTTPHEADER])) {
            $existing_headers = $opts[CURLOPT_HTTPHEADER];
            $existing_headers[] = 'Expect:';
            $opts[CURLOPT_HTTPHEADER] = $existing_headers;
        } else {
            $opts[CURLOPT_HTTPHEADER] = array('Expect:');
        }

        curl_setopt_array($ch, $opts);
        $result = curl_exec($ch);

        if ($result === false) {
            $http_status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
            curl_close($ch);
            throw new Exception("HTTP Error: " . $http_status);
        }
        curl_close($ch);
        return $result;
    }

}

class SearchForm {

    private $api;
    private $form;

    function __construct($api, $form) {
        $this->api = $api;
        $this->form = $form;
    }

    function query($ref, $q = null) {
        if (property_exists($ref, "ref")) {
            $ref = $ref->ref;
        }
        $queryParameters = array();
        foreach($this->form->fields as $key=>$field) {
            if (property_exists($field, "default")) {
                $queryParameters->$key = $field->default;
            }
        }
        $queryParameters["ref"] = $ref;
        if ($q) {
            $queryParameters["q"] = $q;
        }
        print "URL = " . $this->form->action . "?" . http_build_query($queryParameters) . "\n";
        $jsonDocList = json_decode(API::get($this->form->action . "?" . http_build_query($queryParameters)));
        $documents = array();
        foreach($jsonDocList as $jsonDoc) {
            array_push($documents, new Document($jsonDoc));
        }
        return $documents;
    }

}

class Document {

    private $data;

    function __construct($data) {
        $this->data = $data;
    }

    public function id() {
        return $this->data->id;
    }

    public function slug() {
        return $this->data->slugs[0];
    }

}
