<?php

const ERROR_SUCCESS=0;
const ERROR_TOKEN_TIMEOUT=-1;
const SERR_TOKEN_TIMEOUT="您的访问令牌已过期，请重新获取令牌";
const SHELP_TOKEN_TIMEOUT="尝试重新获取令牌再执行操作";
const ERROR_BAD_VALUE=-2;
const SERR_BAD_VALUE="%s 不是一个有效的 %s 值";
const SHELP_BAD_VALUE="检查相关文档以获取正确的参数值范围";
const ERROR_MISSED=-3;
const SERR_MISSED="需要的参数 %s 未提供";
const SHELP_MISSED="提供相应的参数并赋正确的值";
const ERROR_UNSUPPORT=-4;
const SERR_UNSUPPORT="%s 不受支持";
const SHELP_UNSUPPORT="请检查各个参数的值，以确定是否在支持的范围内";
const ERROR_CONNECTION_OPEN=-5;
const ERROR_DATABASE=-6;
const ERROR_SIGN_MISMATCH=-7;
const SERR_SIGN_MISMATCH="参数签名无效";
const SHELP_SIGN_MISMATCH="请检查您的签名算法是否与服务器端一致";
const ERROR_TOKEN_NOT_EXISTS=-8;
const SERR_TOKEN_NOT_EXISTS="token %s 不存在";
const SHELP_TOKEN_NOT_EXISTS="请检查 token 参数的值是否正确";
const ERROR_DATABASE_MISSED=-9;
const SERR_DATABASE_MISSED="指定的数据库别名 %s 不存在。";
const SHELP_DATABASE_MISSED="请检查指定的数据库别名与服务器端是否一致";
const ERROR_PROTOCOL_UNSUPPORT=-10;
const SERR_PROTOCOL_UNSUPPORT="当前协议不受支持";
const SHELP_PROTOCOL_UNSUPPORT="请尝试更换协议或者检查服务器端 EnableHttp/EnableHttps 的参数设置";
const ERROR_ACCESS_DENY=-11;
const SERR_SAVE_TOKEN="无法保存会话状态，权限不足？";
const SHELP_SAVE_TOKEN="请联系服务器管理员检查目录 %s 的访问权限和磁盘空间使用情况。";
const ERROR_ALIAS_MISSED=-12;
const SERR_ALIAS_MISSED="别名 %s 未找到。";
const SHELP_ALIAS_MISSED="请检查别名是否在服务器上存在，或者与服务器大小写一致";

/*数据库别名定义，默认情况下，客户端传入别名，服务器端根据别名对应的连接字符串，建立实际的数据库连接，以完成后续操作
 *如果启用 ENABLE_CONNECTION_STRING 选项，则可以直接传递连接字符串，但必需是服务器端支持的字符串
 */

$KnownDatabases=array(
    "PgSQLDemo"=>array(
        "Database"=>array(
            "Connection"=>"pgsql:host=blog.qdac.cc;port=15432;dbname=QDAC_Demo",//替换为你自己的连接字符串
            "User"=>"你自己的账户",
            "Password"=>"你自己的密码",
            "Attrs"=>array()
        ),
        "Config"=>array(
            //是否支持 Https 协议
            "EnableHttps"=>true,
            //是否支持 HTTP 协议
            "EnableHttp"=>true,
            //是否检查客户端的时间戳和服务器端的时间戳差异
            "CheckTimestamp"=>false,
            //是否允许客户端直接执行SQL命令，如果不允许，则客户端传上来的只能是预定义的SQL名称($KnownSQL 中定义的脚本)
            "EnableSQL"=>false
        )

    ),
    "MySQLDemo"=>array(
        "Database"=>array(
            "Connection"=>"mysql:dbname=qdac;host=127.0.0.1;charset=utf8",//替换为你自己的连接字符串
            "User"=>"你自己的账号",
            "Password"=>"你自己的密码",
            "Attrs"=>array()
        ),
        "Config"=>array(
            //是否支持 Https 协议
            "EnableHttps"=>true,
            //是否支持 HTTP 协议
            "EnableHttp"=>true,
            //是否检查客户端的时间戳和服务器端的时间戳差异
            "CheckTimestamp"=>false,
            //是否允许客户端直接执行SQL命令，如果不允许，则客户端传上来的只能是预定义的SQL名称($KnownSQL 中定义的脚本)
            "EnableSQL"=>false
        )
    )
);
//预定义的SQL脚本，客户端只能传参数，而不能传命令时使用，客户端要直接执行SQL，应启用 EnableSQL 选项，数据库本身的EnableSQL配置会覆盖全局 $Config 的配置
$KnownSQL=array(     
    "Information_Schema.Columns"=>"select * from information_schema.columns",
    "Information_Schema.Tables"=>"select * from information_schema.tables"
);

//预定义 AppId 与 AppSalt 对应关系，实际实现从数据库里取
$KnownAppSalts=array(
    'Demos.HttpProvider'=> 'Demos.HttpProvider.Key'
);

$Config=array(
    //默认会话信息保存路径，实际使用时建议保存到数据库，尤其是在使用负载均衡时，数据文件无法保证下次访问仍在该机器上访问
    "TokenPath"=>__DIR__."/sessions",
    //默认会话超时时间：2小时
    "AccessTokenExpire"=>7200,
    //默认会话刷新时间：30天，30天内刷新可以保持原来的token不变
    "RefreshTokenExpire"=>2592000,

    //是否允许客户端定制连接字符串，如果不支持，则打开连接时，只能database参数指向预定义的数据库别名
    "EnableConnectionString"=>false,
    //下面的是全局配置，可以被单个数据库的配置覆盖
    //是否支持 Https 协议
    "EnableHttps"=>true,
    //是否支持 HTTP 协议
    "EnableHttp"=>true,
    //是否检查客户端的时间戳和服务器端的时间戳差异
    "CheckTimestamp"=>false,
    //是否允许客户端直接执行SQL命令，如果不允许，则客户端传上来的只能是预定义的SQL名称($KnownSQL 中定义的脚本)
    "EnableSQL"=>false
);
