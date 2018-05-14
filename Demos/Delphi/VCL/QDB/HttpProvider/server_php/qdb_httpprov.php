<?php
/**
 * HTTP Provider for QDAC.QDB version 1.0
 * Author:swish
 */

require_once "config.inc";

function RandomStr($length)
{
// 密码字符集，可任意添加你需要的字符
    $chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_";
    $result = "";
    for ($i = 0; $i < $length; $i++) {
        $result .= $chars[mt_rand(0, strlen($chars) - 1)];
    }
    return $result;
}

class QAccessToken
{
    public $AccessToken, $RefreshToken, $AccessExpire, $RefreshExpire, $AppId, $AppSalt,$CreateTime,$TimeOffset,$Data;
    public function __construct()
    {
        if(Key_exists('appid',$_REQUEST))
            $this->AppId=$_REQUEST['appid'];
    }

    public function Assert()
    {
        if (key_exists('sign', $_GET)) {
            $params = $_GET;
            ksort($params);
            $clientsign = $params['sign'];
            $s = '';
            foreach ($params as $k => $v) {
                if ($k != 'sign') {
                    $s = $s .$k.'='.$v.'&';
                }
            }
            $s=$s.$this->AppSalt;
            $serversign=md5($s);
            if($serversign==$clientsign)
                return;
        }
        echo json_encode(array(
            "code" => ERROR_BAD_VALUE,
            "hint" => SERR_SIGN_MISMATCH,
            "help" => SHELP_SIGN_MISMATCH
        ));
        die;
    }
    public function Load($AccessToken)
    {
        global $Config;
        $filename=$Config["TokenPath"].DIRECTORY_SEPARATOR.$AccessToken;
        if(file_exists($filename)) {
            $file = fopen($filename, "r");
            $token = json_decode(fgets($file),true);
            $this->AppId = $token["app"]["id"];
            $this->AppSalt = $token["app"]["salt"];
            $this->AccessToken = $AccessToken;
            $this->RefreshToken = $token["refresh_token"];
            $this->CreateTime = $token["timestamp"];
            $this->AccessExpire = $this->CreateTime + $token["access_expire"];
            $this->RefreshExpire = $this->RefreshExpire + $token["refresh_expire"];
            $this->TimeOffset = $token["time_offset"];
            $this->Data=$token["data"];
            fclose($file);
        }
        else {
            echo json_encode(
                array(
                    "code" => ERROR_TOKEN_NOT_EXISTS,
                    "hint" => sprintf(SERR_TOKEN_NOT_EXISTS,$AccessToken),
                    "help" => SHELP_TOKEN_NOT_EXISTS
                )
            );
            die;
        }
    }

    public function Refresh()
    {
        global $Config;
        $this->CreateTime=time();
        $this->AccessExpire = $this->CreateTime + $Config["AccessTokenExpire"];
        $this->RefreshExpire = $this->CreateTime + $Config["RefreshTokenExpire"];
        $this->TimeOffset = $this->CreateTime - $_REQUEST["timestamp"];
        $this->Save();
    }

    public function Reset()
    {
        global $Config;
        $this->AccessToken = RandomStr(32);
        $this->CreateTime = time();
        $this->AccessExpire = $this->CreateTime + $Config["AccessTokenExpire"];
        $this->RefreshToken=RandomStr(32);
        $this->RefreshExpire = $this->CreateTime + $Config["RefreshTokenExpire"];
        $this->Data=array();
        if (isset($_REQUEST["timestamp"]))
            $this->TimeOffset = $this->CreateTime - $_REQUEST["timestamp"];
        else
            $this->TimeOffset = 0;
    }

    public static function NewToken($AppId,$AppSalt)
    {
        $Result = new QAccessToken();
        $Result->AppId = $AppId;
        $Result->AppSalt = $AppSalt;
        $Result->Reset();
        return $Result;
    }

    public function Invalidate()
    {
        global $Config;
        $filename=$Config["TokenPath"].DIRECTORY_SEPARATOR.$this->AccessToken;
        if(file_exists($filename))
            unlink($filename);
        $this->AccessToken=null;
        $this->CreateTime=null;
        $this->AccessExpire = null;
        $this->RefreshToken=null;
        $this->RefreshExpire = null;
        $this->Data=array();
        $this->TimeOffset=null;
    }
    public function Valid()
    {
        global $Config;
        $now=time();
        if ($now > $this->AccessExpire) {
            echo json_encode(array(
                "code" => ERROR_TOKEN_TIMEOUT,
                "hint" => SERR_TOKEN_TIMEOUT,
                "help" => SHELP_TOKEN_TIMEOUT
            ));
            //刷新Token也过期了的话，删除。同时为了避免避免垃圾产生，应创建一个删除超期会话的作业
            if($now>$this->RefreshExpire){
                unlink($Config["TokenPath"].DIRECTORY_SEPARATOR.$this->AccessToken);
            }
            die;
        }
    }

    public function Save()
    {
        global $Config;
        $this->Valid();
        $now=time();
        $data = array(
            "app" => array(
                "id" => $this->AppId,
                "salt" => $this->AppSalt
            ),
            "access_expire" => $this->AccessExpire - $now,
            "refresh_token" => $this->RefreshToken,
            "refresh_expire" => $this->RefreshExpire - $now,
            "timestamp" => $this->CreateTime,
            "time_offset" => $this->TimeOffset,
            "data"=>$this->Data
        );
        //Token 信息可以写到数据库，这里写入到临时目录下
        $filename=$Config["TokenPath"] . DIRECTORY_SEPARATOR . $this->AccessToken;
        $file = fopen($filename, "w");
        if($file) {
            fputs($file, json_encode($data));
            fclose($file);
        }
        else{
            echo json_encode(
                array(
                    "code"=>ERROR_ACCESS_DENY,
                    "hint"=>SERR_SAVE_TOKEN,
                    "help"=>sprintf(SHELP_SAVE_TOKEN,$Config["TokenPath"])
                )
            );
            die;
        }
    }
};

class TQHttpProvider
{
    private $Token,$Connection,$Driver;
    private function CheckParams($NeedParams){
        foreach ($NeedParams as $v)
        {
            if(!key_exists($v,$_REQUEST))
            {
                echo json_encode(
                    array(
                        "code"=>ERROR_MISSED,
                        "hint"=>sprintf(SERR_MISSED,$v),
                        "help"=>SHELP_MISSED
                    )
                );
                die;
            }
        }
        //检查参数签名

    }
    public function __construct()
    {
        $this->Token=new QAccessToken();
    }

    private function ValidAppSalt()
    {
        //AppId -> AppSalt的对应关系应直接从数据库或者缓存中查找，然后用于验证签名
        if (key_exists("appid", $_REQUEST)) {
            $this->Token->AppId = $_REQUEST["appid"];
            global $KnownAppSalts;
            if (key_exists($this->Token->AppId ,$KnownAppSalts)) {
                $this->Token->AppSalt = $KnownAppSalts[$this->Token->AppId];
                $this->Token->Assert();
            } else {
                echo json_encode(
                    array(
                    "code" => ERROR_BAD_VALUE,
                    "hint" => sprintf(SERR_BAD_VALUE,$this->Token->AppId,"AppId"),
                    "help" => SHELP_BAD_VALUE
                    )
                );
            }
        } else if (key_exists("token", $_REQUEST)) {
            $this->Token->Load($_REQUEST["token"]);
            $this->Token->Assert();
            $this->Token->Valid();
        } else {
            echo json_encode(
                array(
                    "code" => ERROR_MISSED,
                    "hint" => sprintf(SERR_MISSED,"appid/token"),
                    "help" => SHELP_MISSED
                )
            );
            die;
        }
    }
    private function OpenConnection($params){
        $connectionString=$params["Connection"];
        $DriverName=explode(':',$connectionString,2);
        if(count($DriverName)>0)
            $this->Driver=$DriverName[0];
        else
            $this->Driver="unknown";
        return new PDO($connectionString, $params["User"], $params["Password"], $params["Attrs"]);
    }

    private function IsHttps()
    {
        return (!empty($_SERVER['HTTPS']) && strtolower($_SERVER['HTTPS']) !== 'off') || (isset($_SERVER['HTTP_X_FORWARDED_PROTO']) && $_SERVER['HTTP_X_FORWARDED_PROTO'] === 'https')
            || (!empty($_SERVER['HTTP_FRONT_END_HTTPS']) && strtolower($_SERVER['HTTP_FRONT_END_HTTPS']) !== 'off');
    }

    private function Prepare($checkConnection,$checkRequest,$needParams)
    {
        global $Config;
        $this->CheckParams($needParams);
        $this->ValidAppSalt();
        try {
            if(key_exists("Config",$this->Token->Data))
                $localConfig=$this->Token->Data["Config"];
            else
                $localConfig=$Config;
            if ($localConfig["CheckTimestamp"]) {
                $timestamp = $_REQUEST["timestamp"];
                $delta = $timestamp + $this->Token->TimeOffset - time();
                if (abs($delta) > $localConfig["MaxTimeOffset"])
                    $this->Cleanup(ERROR_BAD_VALUE, sprintf(SERR_BAD_VALUE, $timestamp), SHELP_BAD_VALUE, null);
            }

            if (($this->IsHttps() && $localConfig["EnableHttps"]) or $localConfig["EnableHttp"]) {
                if ($checkConnection) {
                    $DBParams = $this->Token->Data["Database"];
                    $this->Connection = $this->OpenConnection($DBParams);
                    $this->Connection->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
                    if ($checkRequest) {
                        if ($_SERVER["REQUEST_METHOD"] == "POST")
                            $result = json_decode(file_get_contents("php://input"), true);
                        else {
                            $result = array();
                            if (key_exists("sql", $_REQUEST))
                                $result["sql"] = $_REQUEST["sql"];
                            if (key_exists("transaction", $_REQUEST))
                                $result["transaction"] = $_REQUEST["transaction"];
                        }
                        if (isset($result) && key_exists("sql", $result)) {
                            if ($result["transaction"])
                                $this->Connection->beginTransaction();
                            return $result;
                        } else {
                            $this->Cleanup(ERROR_MISSED, sprintf(SERR_MISSED, "sql"), SHELP_MISSED, null);
                        }
                    }
                }
            } else
                $this->Cleanup(ERROR_PROTOCOL_UNSUPPORT, SERR_PROTOCOL_UNSUPPORT, SHELP_PROTOCOL_UNSUPPORT, null);
        } catch (Exception $e) {
            $this->Cleanup(ERROR_DATABASE, $e->getMessage(), $e->getTraceAsString(), null);
        }
        return array();
    }

    private function Cleanup($code,$msg,$help,$result)
    {
        if (isset($this->Connection)&&$this->Connection->inTransaction()) {
            if ($code==ERROR_SUCCESS)
                $this->Connection->commit();
            else
                $this->Connection->rollBack();
        }
        if($code==ERROR_SUCCESS )//操作成功完成
            echo json_encode(
                array(
                "code"=>$code,
                "result"=>$result
                )
            );
        else
            echo json_encode(
                array(
                "code"=>$code,
                "hint"=>$msg,
                "help"=>$help
                )
            );
        die;
    }

    public function Open()
    {
        global $KnownDatabases, $Config;
        $this->Prepare(false,false,array("database", "appid", "timestamp", "database"));
        $this->ValidAppSalt();
        $db = $_REQUEST["database"];
        $params = array();
        $dbConfig=array();
        if (key_exists($db, $KnownDatabases))
        {
            $params = $KnownDatabases[$db]["Database"];
            $dbConfig=$KnownDatabases[$db]["Config"];
        }
        else if ($Config["EnableConnectionString"]) {
            $params["Connection"] = $db;
            if (key_exists("user", $_REQUEST))
                $params["User"] = $_REQUEST["user"];
            else
                $params["User"] = "";
            if (key_exists("password", $_REQUEST))
                $params["Password"] = $_REQUEST["password"];
            else
                $params["Password"] = "";
            $params["Attrs"] = array();
            $dbConfig["EnableHttps"]=$Config["EnableHttps"];
            $dbConfig["EnableHttp"]=$Config["EnableHttp"];
            $dbConfig["CheckTimestamp"]=$Config["CheckTimestamp"];
            $dbConfig["EnableSQL"]=$Config["EnableSQL"];
        } else {
            echo json_encode(
                array(
                "code" => ERROR_DATABASE_MISSED,
                "hint" =>sprintf(SERR_DATABASE_MISSED,$db),
                "help" => SHELP_DATABASE_MISSED
                )
            );
            die;
        }
        $this->Token->Reset();
        try {
            $this->Connection = $this->OpenConnection($params);
            $this->Token->Data["Database"] = $params;
            $this->Token->Data["Config"]=$dbConfig;
            $this->Token->Save();
            echo json_encode(
                array(
                "code" => 0,
                "result" => array(
                    "access_token" => $this->Token->AccessToken,
                    "access_expire" => $Config["AccessTokenExpire"],
                    "refresh_token" => $this->Token->RefreshToken,
                    "refresh_expire" => $Config["RefreshTokenExpire"],
                    "timestamp" => time(),
                    "timeoffset"=>$this->Token->TimeOffset,
                    "driver"=>$this->Driver,
                    "server"=>$this->Connection->getAttribute(PDO::ATTR_SERVER_VERSION)
                )
                )
            );
        } catch (Exception $e) {
            echo json_encode(
                array(
                    "code" => ERROR_CONNECTION_OPEN,
                    "hint" => $e->getMessage(),
                    "help" => $e->getTraceAsString()
                )
            );
        }
    }
    public function Close()
    {
    $this->Prepare(false,false,array("token","timestamp"));
    $this->Token->Invalidate();
    $this->Cleanup(0,null,null,null);
    }

    private function fetchColumnMeta($columnMetas)
    {
        $typemap = array();
        if ($this->Driver == 'pgsql') {
            $typemap = array(
                "bool" => "Boolean",
                "char" => "FixedWideChar",
                "name" => "WideString",
                "int8" => "LargeInt",
                "int2" => "SmallInt",
                "int2vector" => "WideString",
                "int4" => "Integer",
                "regproc" => "WideString",
                "text" => "WideMemo",
                "oid" => "LongWord",
                "json" => "WideMemo",
                "xml" => "WideMemo",
                "float4" => "Single",
                "float8" => "Float",
                "money" => "Currency",
                "varchar" => "WideString",
                "date" => "Date",
                "time" => "Time",
                "timestamp" => "DateTime",
                "timestamptz" => "DateTime",
                "interval" => "String",
                "timetz" => "Time",
                "bit" => "Bytes",
                "varbit" => "VarBytes",
                "numeric" => "BCD",
                "refcursor" => "Cursor",
                "uuid" => "Guid",
                "jsonb" => "WideMemo"
            );
        } else if ($this->Driver == 'mysql') {
            $typemap = array(
                "STRING" => "WideString",
                "VAR_STRING" => "WideString",
                "TINY" => "ShortInt",
                "BIT" => "Boolean",
                "SHORT" => "SmallInt",
                "LONG" => "Integer",
                "LONGLONG" => "LargeInt",
                "INT24" => "Integer",
                "FLOAT" => "Single",
                "DOUBLE" => "Float",
                "DECIMAL" => "BCD",
                "NEWDECIMAL" => "BCD",
                "GEOMETRY" => "String",
                "TIMESTAMP" => "DateTime",
                "YEAR" => "SmallInt",
                "ENUM" => "String",
                "DATE" => "Date",
                "NEWDATE" => "Date",
                "TIME" => "Time",
                "DATETIME" => "DateTime",
                "BLOB" => "Blob",
                "TINY_BLOB" => "Blob",
                "MEDIUM_BLOB" => "Blob",
                "LONG_BLOB" => "Blob"
            );
        } else if ($this->Driver = 'sqlite') {//SQLite
            $typemap = array(
                "double" => "Float",
                "integer" => "Integer",
                "string" => "WideString"
            );
        } else if ($this->Driver = 'dblib') {//PDO_DBLib
            $typemap = array(
                'nvarchar' => "WideString",
                'image' => "Blob",
                'text' => "Memo",
                'uniqueidentifier' => "Guid",
                'varbinary' => "VarBytes",
                'bigint' => "LargeInt",
                'varchar' => "String",
                'date' => "Date",
                'time' => "Time",
                'datetime2' => "DateTime",
                'datetimeoffset' => "TimeOffset",
                'binary' => "Bytes",
                'char' => "String",
                'tinyint' => "ShortInt",
                'bit' => "Boolean",
                'smallint' => "SmallInt",
                'decimal' => "BCD",
                'int' => "Integer",
                'smalldatetime' => "DateTime",
                'real' => "Single",
                'money' => "Currency",
                'datetime' => "DateTime",
                'float' => "Float",
                'numeric' => "BCD",
                'sql_variant' => "Blob",
                'ntext' => "WideMemo",
                'smallmoney' => "Currency",
                'timestamp' => "TimeStamp",
                'nchar' => "WideString",
                'geometry' => "WideString",
                'xml' => "WideMemo",
                'unknown' => "Blob"
            );
        }
        $result = array();
        foreach ($columnMetas as $meta) {
            switch ($meta["pdo_type"]) {
                case PDO::PARAM_BOOL:
                    $meta["delphi_type"] = 'Boolean';
                    break;
                case PDO::PARAM_INT:
                    switch ($meta["length"]) {
                        case 1:
                            $meta["delphi_type"] = "ShortInt";
                            break;
                        case 2:
                            $meta["delphi_type"] = "SmallInt";
                            break;
                        case 4:
                            $meta["delphi_type"] = "Integer";
                            break;
                        case 8:
                            $meta["delphi_type"] = "LargeInt";
                            break;
                    }
                    break;
                case PDO::PARAM_STR:
                    $meta["delphi_type"] = "WideString";
                    break;
            }
            if (key_exists("native_type", $meta)) {
                $nativeType = $meta["native_type"];
                if (key_exists($nativeType, $typemap))
                {
                    $meta["delphi_type"] = $typemap[$nativeType];
                    if($meta["delphi_type"]=="Blob")
                    {
                        if($meta["pdo_type"]==PDO::PARAM_STR)
                            $meta["delphi_type"]="WideMemo";
                    }
                }
            }
            array_push($result, $meta);
        }
        return $result;
    }

    public function OpenDataSet()
    {
        global $Config, $KnownSQL;
        $req = $this->Prepare(true, true, array("token", "timestamp"));
        try {
            $sql = $req["sql"];
            if ($Config["EnableSQL"]) {
                if (key_exists($sql, $KnownSQL))
                    $sql = $KnownSQL[$sql];
            } else if (key_exists($sql, $KnownSQL))
                $sql = $KnownSQL[$sql];
            else
                $this->Cleanup(ERROR_ALIAS_MISSED,sprintf(SERR_ALIAS_MISSED,$sql),SHELP_ALIAS_MISSED,null);
            $statement = $this->Connection->prepare($sql);
            $params=array();
            if(key_exists('params',$req))
                $params=$req["params"];
            $c=count($params);
            if($c>0) {
                for ($i = 0; $i < $c; $i++) {
                    foreach ($params[$i] as $name => $value) {
                        if (is_int($name))//名称是整数时，认为是占位数
                            $statement->bindValue($name, $value);
                        else
                            $statement->bindValue(':' . $name, $value);
                    }
                    $statement->execute();
                }
            }
            else
                $statement->execute();
            $rows = $statement->fetchAll(PDO::FETCH_NUM);
            $cols = array();
            for ($i = 0; $i < $statement->columnCount(); $i++)
                array_push($cols, $statement->getColumnMeta($i));
            $cols=$this->fetchColumnMeta($cols);
            $statement->closeCursor();
            $this->Cleanup(ERROR_SUCCESS, null, null, array(
                "Fields" => $cols,
                "Records" => $rows
                )
            );

        } catch (Exception $e) {
            $this->Cleanup(ERROR_DATABASE, $e->getMessage(), $e->getTraceAsString(), null);
        }
    }

    public function ExecSQL()
    {
        global $Config,$KnownSQL;
        $req = $this->Prepare(true,true,array("token","timestamp"));
        try {
            $sql = $req["sql"];
            if($Config["EnableSQL"]) {
                if (key_exists($sql, $KnownSQL))
                    $sql = $KnownSQL[$sql];
            }
            else if (key_exists($sql,$KnownSQL))
                $sql=$KnownSQL[$sql];
            else
                $this->Cleanup(ERROR_ALIAS_MISSED,sprintf(SERR_ALIAS_MISSED,$sql),SHELP_ALIAS_MISSED,null);
            $statement=$this->Connection->prepare($sql);
            $params=array();
            if(key_exists('params',$req))
                $params=$req["params"];
            $c=count($params);
            if($c>0) {
                for ($i = 0; $i < $c; $i++) {
                    foreach ($params[$i] as $name => $value) {
                        if (is_int($name))//名称是整数时，认为是占位数
                            $statement->bindValue($name, $value);
                        else
                            $statement->bindValue(':' . $name, $value);
                    }
                    $statement->execute();
                }
            }
            else
                $statement->execute();
            $this->Cleanup(ERROR_SUCCESS, null, null, $statement->rowCount());
        } catch (Exception $e) {
            $this->Cleanup(ERROR_DATABASE, $e->getMessage(), $e->getTraceAsString(), null);
        }
    }

    public function RefreshToken()
    {
        global $Config;
        $this->CheckParams(array('token', 'refreshtoken', 'timestamp'));
        $this->Token->Load($_REQUEST["token"]);
        if ($this->Token->RefreshExpire > time()) {
            $this->Token->Refresh();
            echo json_encode(
                array(
                    "code" => 0,
                    "result" => array(
                        "access_token" => $this->Token->AccessToken,
                        "access_expire" => $Config["AccessTokenExpire"],
                        "refresh_token" => $this->Token->RefreshToken,
                        "refresh_expire" => $Config["RefreshTokenExpire"],
                        "timestamp" => time(),
                        "timeoffset" => $this->Token->TimeOffset,
                    )
                )
            );
        } else
            $this->Token->Valid();
    }
    public function Dispatch()
    {
        $actions = explode('/', $_SERVER["PATH_INFO"]);
        if (!empty($actions)) {
            if (method_exists($this, $actions[1])) {
                call_user_func(array($this, $actions[1]));
                die;
            }
        }
        if (count($actions) > 1)
            echo json_encode(
                array(
                    "code" => ERROR_UNSUPPORT,
                    "hint" => sprintf(SERR_UNSUPPORT,$actions[1]),
                    "help" => SHELP_UNSUPPORT
                )
            );
        else
            echo json_encode(
                array(
                    "code" => ERROR_MISSED,
                    "hint" => sprintf(SERR_MISSED,"Action"),
                    "help" => SHELP_MISSED
                )
            );
        die;
    }
};

header("Content-Type:application/json;charset=UTF-8");
$Provider=new TQHttpProvider();
$Provider->Dispatch();
