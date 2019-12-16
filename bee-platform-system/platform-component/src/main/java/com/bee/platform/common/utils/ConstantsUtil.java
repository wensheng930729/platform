package com.bee.platform.common.utils;

/**
 * notes
 * author junyyang.li
 * create 2018/11/6 0006 10:11
 **/
public class ConstantsUtil {

    public final static String COLON = ":";


    public final static String COMMA = ",";
    public final static String SLASH = "/";
    public final static String UNDERLINE = "_";
    public final static String LINE = "-";
    public final static String PERCENT = "%";
    public final static String DOUBLE_PERCENT = "%%";
    public final static String STAR = "*";
    public final static String QUESTION="?";
    public final static String EQUAL="=";


    /** 15分钟*/
    public static final int SECOND=900;
    /** 5分钟*/
    public static final int FIFTEEN_MINUTE=900;

    /** 信息保存时间单位秒 */
    public static final int OVERDUE=86400;
    public static final long ONE_DAY=86400L;
    /**权限拦截开关，0为false ，1为true*/
    public static final String FALSE = "0";
    public static final String TRUE = "1";
    public static final String MINUS="-1";
    public final static String UTF8="UTF-8";
    public static final String CONTENT_TYPE="application/json;charset=utf-8";
    public static final String[] IMAGE_FORMAT=new String[]{".png",".PNG",".jpg",".JPG",".jpeg",".JPEG"};

    /**平台企业id*/
    public static final String ORG_ID = "orgId";
    public final static String OPTIONS="OPTIONS";
    public final static String SYS_TOKEN="sysToken";
    public final static String OK="OK";
    public static final String PLATFORM="platform-";
    /** 发送短信验证码*/
    public static final String CODE="code";
    public static final String ORG_NAME="org_name";
    public static final String SYS_GROUPID="sub_sys_clientid";
    public static final String SUB_GROUPID="subSysClientid";
    /** 平台区分中台，后台的请求标识*/
    public static final String PLATFORM_PERMISSION_CLIENT_ID="platform_permission_client_id";
    /** 配置表中配置的SysToken失效时间*/
    public static final String SYSTOKEN_VALID_TIME="sysToken_valid_time";
    //内部服务请求的标识
    public static final String INNER_CLIENT_ID="innerClientId";
    public static final String FINANCE_USER="platform-user";


    /** redis键*/
    public static final String SYSTEM_ALL_ROLE="platform_all_role";
    public static final String VALIDATE_CODE="validate_code_";
    public static final String VALIDATE_RESULT="validate_result_";
    public static final String ALL_TEST_ACCOUNT_KEY="platform_test_account_all";
    public static final String ALL_REGION_KEY="platform_all_region";
    public static final String ALL_REGION_TREE_NODE_KEY="platform_all_region_tree_node";
    public static final String ALL_RESOURCE_KEY="platform_all_resource";
    public static final String BUSINESS_ID_KEY="platform_business_id_";
    public static final String COMMON_AUTH_USER_URL="common_auth:user:";
    public static final String COMMON_AUTH_ENTERPRISE_URL="common_auth:enterprise:";
    public static final String COMMON_AUTH_WHITELIST_KEY = "common_auth:whitelist:";
    public static final String PURCHASE_GOODS_ID_KEY="purchase_goods_id_";
    public static final String LOGIN_COUNT_KEY="login_count_key_";




    /** 极验第一次校验的结果key*/
    public static final String GEETEST_FRSIT_VERIFY="platform_geetest_status_";

    /**管理后台的redis键 */
    public static final String MANAGER_ALL_ROLE="platform_all_manager_role";
    public static final String MANAGER_USER_ROLE="platform_manager_roleIds_user_";
    public static final String ALL_NOTICE_TEMPLATE="manager_all_notice_template";
    public static final String MANAGER_ALL_RESOURCE="manager_all_resource";
    public static final String MANAGER_ALL_BUTTON="manager_all_button";
    /** 角色对应的菜单**/
    public static final String ROLE_MENU_TREE="manager_menu_tree_";
    /** 角色对应的按钮**/
    public static final String ROLE_BUTTON="manager_button_";
    public static final String ALL_MANAGER_INTERFACE_KEY="all_manager_interface_key";

    /** 码表组*/
    public static final String TEST_ACCOUNT="test_account";
    public static final String PLATFORM_SYSTEMCODE_KEY_PREFIX= "platform:systemcode:";
    public static final String CUSTOMER_FIRST_CATEGORY= "customer_category_first";



    /** 配置表*/
    /** 权限认证开关*/
    public static final String TOKEN_EXPIRES="finance_token_expires_time";
    public static final String AUTH_SWITCH="bee_trade_user_auth_switch";
    public static final String TOKEN_EXPIRES_IN="platform_token_expires_in";
    public static final String LOGOUT_SWITCH="logout_switch";
    public static final String DEFAULT_PASSWORD="default_password";
    /** 是否开启码表缓存的键 0不开启，1开启*/
    public static final String IS_ENABLESYSCODECACHE_KEY="supplychainfinance_is_enable_syscodecache_v1.0";
    /** 配置表中是否允许测试账号登陆的开关键*/
    public static final String TEST_ACCOUNT_LOGIN="test_account_login";
    /** 邮件 */
    public static final String V_EMAIL="^\\w+((-\\w+)|(\\.\\w+))*\\@[A-Za-z0-9]+((\\.|-)[A-Za-z0-9]+)*\\.[A-Za-z0-9]+$";

    
    /** 华为obs对象存储返回参数*/
    public final static String OBJECTURL = "objectUrl";
    /** 华为obs对象存储返回参数*/
    public final static String ETAG = "etag";

    /** 新老权限系统数据迁移开关*/
    public static final String PERMISSION_DATA_SWITCH="permission_data_switch";

    /**新权限母公司不充钱可添加的子公司数目*/
    public static final String INITIAL_COMPANY_NUM="initial_company_num";
    /** 金蜜默认头像 */
    public final static String DEFAULT_HEAD = "https://beesrv-1252637635.file.myqcloud.com/b202f805770642f39933ba95f5e7acc9.png";

    /** 配置是否走缓存的全局开关：1走缓存,0不走缓存 */
    public static final String GLOBALE_CONFIG_SWITCH = "globale_config_switch";

    //
    public final static String COMPANY="company";
}
