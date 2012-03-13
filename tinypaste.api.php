<?php

/**
* TinyPaste API
*
* These methods allow connection to TinyPaste's API
*
* @author Cole Turner <turner.cole@gmail.com>
* @version 1.0
* @since 1.0
* @access public
* @copyright TinyPaste
*/ 

/**
* API Handler Functions
*/
class TinyPasteAPI
{

	/**
	 * Holds the url to the API
	 *
	 * @access	public
	 * @var		string
	 */
	public $api_url = "http://tinypaste.com/api/";

	/**
	 * Sends the data to TinyPaste and retrieves a response
	 *
	 * @access	protected
	 * @param	string	$call
	 * @param	array	$data
	 * @param	string	$http
	 * @param	string	$format
	 * @return	string	Reponse from TinyPaste API
	 */
	protected function call($call, $data, $http='POST', $format='xml')
	{
		if($call != "create" && $call != "edit" && $call != "delete" && $call != "get")
		{
			trigger_error("API call() method '".$call."' is not supported");
		}
		
		$data = $this->processData($data);
		
		$url = $this->api_url . $call . ".".$format;
		$ch = curl_init();
		
		if(strtoupper($http) == 'GET')
		{
			$url .= "?".$data;
		}
	
		curl_setopt($ch, CURLOPT_URL,$url);
		curl_setopt($ch, CURLOPT_HEADER, 0);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
		
		if(strtoupper($http) == 'POST')
		{
			curl_setopt($ch, CURLOPT_POST, 1);
			curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
		}
		else
		{
			curl_setopt($ch, CURLOPT_POST, 0);
		}
	
		$reply = curl_exec ($ch);
		
		curl_close ($ch);
		return $reply;
	}
	

	/**
	 * Makes an array into an URL-Friendly string
	 *
	 * @access	private
	 * @param	array	$data
	 * @return	string	URL-Friendly version of array
	 */
	private function processData($data)
	{
		$out = '';
		
		if(!is_array($data))
		{
			trigger_error("API call() data should be an array");
		}
		
		foreach($data as $k => $v)
		{
			$out .= urlencode($k)."=".urlencode($v)."&";
		}
		
		return $out;
	}

	/**
	 * Shortens a string, adding "..."
	 *
	 * @access	public
	 * @param	string	 $str
	 * @param	integer	 $length
	 * @param	integer	 $minword
	 * @return	string	 Shortened string
	 */
	public function _substr($str, $length, $minword = 3)
	{
		$sub = '';
		$len = 0;
		
		foreach (explode(' ', $str) as $word)
		{
			$part = (($sub != '') ? ' ' : '') . $word;
			$sub .= $part;
			$len += strlen($part);
			
			if (strlen($word) > $minword && strlen($sub) >= $length)
			{
				break;
			}
		}
		
		return $sub . (($len < strlen($str)) ? '...' : '');
	}
	

	/**
	 * Processes XML into an Array
	 *
	 * @access	public
	 * @param	string	 $originalXML
	 * @param	boolean	 $attributes
	 * @return	array	 Array containing XML
	 */
	protected function xml2array($originalXML, $attributes=true)
	{
	        $xmlArray = array();
	        $search = $attributes ? '|<((\S+)(.*))\s*>(.*)</\2>|Ums' : '|<((\S+)()).*>(.*)</\2>|Ums';
	       
	        // normalize data
	        $xml = preg_replace('|>\s*<|', ">\n<", $originalXML); // one tag per line
	        $xml = preg_replace('|<\?.*\?>|', '', $xml);            // remove XML declarations
	        $xml = preg_replace('|<(\S+?)(.*)/>|U', '<$1$2></$1>', $xml); //Expand singletons
	       
	        if (! preg_match_all($search, $xml, $xmlMatches))
	                return trim($originalXML);      // bail out - no XML found
	               
	        foreach ($xmlMatches[1] as $index => $key)
	        {
	                if (! isset($xmlArray[$key])) $xmlArray[$key] = array();       
	                $xmlArray[$key][] = $this->xml2array($xmlMatches[4][$index], $attributes);
	        }
	        return $xmlArray;
	}

}

/**
* API Call Instance
*/
class TinyPaste extends TinyPasteAPI
{
	/**
	 * Holds the paste information
	 *
	 * @access	private
	 * @var		array
	 */
	private $paste = array();
	
	/**
	 * Holds which API we will call
	 *
	 * @access	private
	 * @var		string
	 */
	private $call = 'create';
	

	/**
	 * Designates which API to call
	 *
	 * @access	public
	 * @return	void
	 */
	public function __construct($call='create')
	{
		$this->call = $call;
	}
	
	/**
	 * Executes the API Call
	 *
	 * @access	public
	 * @param	string	 $format
	 * @param	string	 $method
	 * @param	boolean	 $unparse
	 * @return	array    Response from API
	 */
	public function execute($format="xml", $method='POST', $unparse=true)
	{
		$response = $this->call($this->call, $this->paste, $method, $format);
		
		if($unparse)
		{
			if($format == 'xml')
			{
				return $this->xml2array($response, false);
			}
			else
			{
				return json_decode($response);
			}
		}
	}
	
	/**
	 * Sets the paste #id
	 *
	 * @access	public
	 * @param	string	 $str
	 * @return	boolean  true
	 */
	public function id($str)
	{
		$this->paste['id'] = $str;
		return true;
	}
	
	/**
	 * Sets the paste
	 *
	 * @access	public
	 * @param	string	 $str
	 * @return	boolean  true
	 */
	public function paste($str)
	{
		$this->paste['paste'] = $str;
		return true;
	}

	/**
	 * Sets a title for the paste
	 *
	 * @access	public
	 * @param	string	 $str
	 * @return	boolean  true
	 */
	public function title($str)
	{
		$this->paste['title'] = $this->_substr($str, 40);
		return true;
	}
	
	/**
	 * Sets the paste under subdomain: $str.tinypaste.com
	 *
	 * @access	public
	 * @param	string	 $str
	 * @return	boolean
	 */
	public function subdomain($str)
	{
		if(strlen($str) < 3 || strlen($str) > 15)
		{
			return false;
		}
		
		$this->paste['subdomain'] = $str;
		return true;
	}
	
	/**
	 * Sets the paste to be a Code paste
	 *
	 * @access	public
	 * @param	boolean	 $bool
	 * @return	boolean  true
	 */
	public function isCode($bool)
	{
		$bool = ($bool == true || $bool == 1 ? "1" : "0");
		$this->paste['is_code'] = $bool;
		return true;
	}
	
	/**
	 * Sets the paste to be private
	 *
	 * @access	public
	 * @param	boolean	 $bool
	 * @return	boolean  true
	 */
	public function isPrivate($bool)
	{
		$bool = ($bool == true || $bool == 1 ? "1" : "0");
		$this->paste['is_private'] = $bool;
		return true;
	}
	
	
	/**
	 * Sets the password for the paste
	 *
	 * @access	public
	 * @param	string	 $str
	 * @return	boolean  true
	 */
	public function setPassword($str)
	{
		$this->paste['password'] = $str;
		return true;
	}
	
	/**
	 * Sets the paste to expire
	 *
	 * @access	public
	 * @param	integer	 $int
	 * @param	string	 $unit
	 * @return	void
	 */
	public function expires($int=30, $unit='minute')
	{
		$int = intval($int);
		if($int == '0')
		{
			$this->paste['expires'] = 0;
		}
		else
		{
			$x = time();
			if($unit == 'second')
			{
				$this->paste['expires'] = ($x + ($int));
			}
			else if($unit == 'minute')
			{
				$this->paste['expires'] = ($x + ($int * 60));
			}
			else if($unit == 'hour')
			{
				$this->paste['expires'] = ($x + ($int * 60 * 60));
			}
			else if($unit == 'day')
			{
				$this->paste['expires'] = ($x + ($int * 86400));
			}
			else if($unit == 'week')
			{
				$this->paste['expires'] = ($x + ($int * 86400 * 7));
			}
			else if($unit == 'month')
			{
				$this->paste['expires'] = ($x + ($int * 86400 * 30));
			}
			else if($unit == 'year')
			{
				$this->paste['expires'] = ($x + ($int * 86400 * 365));
			}
			else
			{
				trigger_error("API expires() invalid unit: ".$unit);
			}
		}
	}
	
	/**
	 * Sets the paste to authenticate
	 *
	 * @access	public
	 * @param	string	 $user
	 * @param	string	 $pass
	 * @return	void
	 */
	public function authenticate($user, $pass)
	{
		if($user != '' && $pass != '')
		{
			$this->paste['authenticate'] = $user.":".$pass;
		}
	}
	

	/**
	 * Sets the paste as a child of parent paste
	 *
	 * @access	public
	 * @param	string	 $id
	 * @return	void
	 */
	public function parent($id)
	{
		$this->paste['parent'] = $id;
	}
}


    
