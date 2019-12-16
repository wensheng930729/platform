package com.bee.platform.common.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author zhigang.zhou
 * @version 1.0.0
 * @ClassName ResCodeEnum
 * @Description (1)、code为0时：
 * 0->请求成功，程序可以继续往下执行；
 * (2)、code为正数时：
 * 1到20->给tost提示，程序可以继续往下执行；
 * 21到40：弹确认框，用户点击确认之后程序可以继续往下执行；
 * 41到60：跳转业务逻辑页面。
 * (3)、code为负数时：
 * 00到0->弹窗错误提示框，并阻止程序继续往下执行；
 * -20到01->跳转错误页面
 * @Date 2018年10月26日 下午1:16:35
 */
@Getter
@AllArgsConstructor
public enum ResCodeEnum {

    /**
     * 返回枚举
     */
    SUCCESS(1, "成功"),
    ERROR_SUBMIT(0, "提交出错,请联系管理员"),
    ERROR_DATA_EXISTED(0, "已存在id对应数据"),
    ERROR_NOT_FOUND(0, "无法找到相应的数据"),
    BUSY_SYSTEM(0, "系统繁忙,请稍后再试"),
    ERROR_SYSTEM(0, "系统内部错误"),
    FAILED(0, "失败"),
    NO_AUTHORITY(-10, "没有权限"),
    USER_ALREADY_EXIST(0, "用户已存在"),
    USER_NOT_EXIST(0, "用户不存在"),
    USER_NOT_ADMIN(0, "用户不是管理员"),
    ADMIN_CANNOT_DELETE_SELF(0, "管理员不能删除自己"),
    USER_ALREADY_ADMIN(0, "用户已经是管理员"),
    ERROR_PARAMETER(0, "参数错误"),
    ERROR_PARAMETER_NO_COMPANY(0, "参数错误,没有获取到公司id"),
    PARAMETER_INCOMPLETE(0, "参数不全"),
    USER_NAME_EXIT(0, "该账户已存在，无需重复注册"),
    HYSTRIX_ENABLED(-100, "无法找到相应的数据，该服务被熔断"),
    UPDATE_FAIL(0, "更新失败"),
    SAVE_FAIL(0, "保存失败"),
    NOT_CONFIRM_TWICE(0, "请勿重复确认"),
    SURE_FAIL(0, "确认失败"),
    NO_DATA(0, "无法找到对应的数据"),
    DELETE_FAIL(0, "删除失败"),
    USER_NOT_AUTHORIZE(0,"用户未授权,请与管理员联系"),
    /**
     * 文章相关
     */
    ARTICLE_NOT_TO_THIS(0, "该文章不属于该企业"),
    ARTICLE_TYPE_ERROR(0, "文章类型错误"),
    ARTICLE_TYPE_NOT_EMPTY(0, "该类型公告下存在公告"),
    ARTICLE_NOT_FOUND(0, "公告不存在"),
    INSERT_ARTICLE_FAILED(0, "文章新增失败"),
    DELETE_ARTICLE_FAILED(0, "文章删除失败"),
    UPDATE_ARTICLE_FAILED(0, "文章编辑失败"),
    UPDATE_ARTICLE_TYPE_FAILED(0, "公告类型编辑失败"),
    GET_ARTICLE_TYPE_FAILED(0, "暂无公告类型"),
    /**
     * 企业相关
     */
    DEPARTMENT_USER_NOT_EXIST(0, "用户无相关部门"),
    NO_ENTERPRISE_ID(0, "从header中未获取到企业id"),
    ONLY_SUB_ENTERPRISE(0, "管理员不可删除自己管理的母公司，只可删除子公司"),
    ENTERPRISE_NOT_EXIST(0, "企业不存在"),
    ENTERPRISE_EXIST(0, "企业已注册"),
    CANCEL_FAIL(0, "取消认证失败"),
    ENTERPRISE_IN_AUDIT(0, "企业在审核中"),
    ENTERPRISE_USER_REPEATED_AUDIT(0, "企业用户重复审核"),
    ENTERPRISE_REPEAT(0, "企业重复"),
    ENTERPRISE_ASSOCIATED_IN_AUDIT(0, "企业关联正在审核中"),
    ENTERPRISE_ALREADY_ASSOCIATED(0, "企业已关联"),
    ENTERPRISE_IS_OK(1, "企业可关联"),
    ENTERPRISE_REGISTERED_IS_OK(1, "企业可注册"),
    DEPARTMENT_NOT_EXIST(0, "部门不存在"),
    POST_NOT_EXIST(0, "职位不存在"),
    DEPARTMENT_SAVE_FAILED(0, "企业部门信息保存异常"),
    POST_SAVE_FAILED(0, "职位信息保存异常"),
    ROOT_DEPARTMENT_DELETE_FAILED(0, "删除失败，根部门不能删除"),
    DEPARTMENT_CANNOT_BE_EMPTY(0, "删除失败，该部门不为空"),
    PARAMETER_MUST_GT_TWO(0, "参数必须大于等于两个"),
    GET_USER_FAILED(0, "用户查询失败"),
    UPDATE_USER_FAILED(0, "用户编辑失败"),
    GET_ENTERPRISE_USER_RELATION_FAILED(0, "用户企业信息查询失败"),
    GET_DEPARTMENT_USER_RELATION_FAILED(0, "用户部门信息查询失败"),
    USER_AT_ALREADY(0, "该用户已在企业和部门中"),
    USER_AT_OTHER(0, "该用户已在其他集团企业和部门中"),
    USER_AT_ALREADY_ENTERPRISE(0, "该用户已在列表中"),
    ENTERPRISE_CHECK_NOT_EXIST(0, "企业认证单不存在"),
    ENTERPRISE_USER_NOT_FOUND(0, "用户不是该企业成员"),
    NOT_THE_ENTERPRISE_USER(0, "非本企业成员,无法获得用户信息"),
    USER_ROLR_SAVE_FAILED(0, "用户角色信息保存异常"),
    EMAIL_ERROR(0, "请输入正确的邮箱地址"),
    NOT_JOIN_ENTERPRISE(0, "用户未加入任何企业"),
    ADD_FAILED(0, "新增失败"),
    FILE_ADD_FAILED(0, "附件新增失败"),
    ENTERPRISE_ALREADY_REGISTER(0, "企业已注册"),
    ENTERPRISE_HAS_CHILD(0, "企业有子公司，不能删除"),
    CHILD_REACH_LIMIT(0, "企业子公司已达上限"),
    CHILD_NUM_NOCONFIG(0,"企业子公司数量未设置"),

    ENTERPRISE_NAME_EXISTED(0, "企业已存在，请重新注册"),
    /**
     * 平台用户统一code
     */
    NOT_PHONE_NUMBER(0, "请输入正确的手机号"),
    CODE_NOT_NULL(0, "验证码不能为空"),
    VALIDATE_CODE_EXPIRE(0, "验证码已过期，请重新发送"),
    UNQUALIFIED_PASSWORD(0, "密码格式不正确，应使用8-18位的数字和字母结合"),
    VALIDATE_CODE_ERROR(0, "验证码错误"),
    PHONE_NUMBER_EXIST(1, "该手机号已注册"),
    PHONE_NO_NUMBER_EXIST(0, "该手机可以注册"),
    PHONE_NUMBER_ATYPISM(0, "输入的手机号与本账号不一致，请重新输入"),
    NOT_VALIDATE(0, "验证码未成功验证"),
    USERNAME_OR_PASSWORD_ERROR(0, "账号或密码错误"),
    SEND_MESSAGE_SUCCESS(1, "验证码发送成功"),
    SEND_MESSAGE_FAIL(0, "验证码发送失败,请稍后重试"),
    CHANGE_SUCCESS(1, "切换成功"),
    FAILED_TO_GET_USER_INFO(401, "登录信息有误，请重新登录"),
    FINANCE_TOKEN_IS_NULL(-2, "登录凭证有误，请重新登录"),
    APP_NOT_OPEN(-2, "未开通该应用"),
    NOT_FOUND_COMPANY(-3, "未获得企业信息"),
    NOT_FOUND_COMPANY_ID(-3, "未获得企业ID"),
    NOT_FOUND_USERNAME(-3, "未获得用户账号"),
    NOT_FOUND_MATCH_USERTYPE(-1, "用户类型错误"),
    SAVE_FAILED(0, "保存失败"),
    UPDATE_FAILED(0, "修改失败"),
    DELETE_FAILED(0, "删除失败"),
    NOT_LOGIN(401, "未登录,请先登录"),
    SYS_TOKEN_NOT_NULL(0, "登录凭证不能为空"),
    USER_ENTERPRISES_NOT_FOUND(0, "该用户不在当前企业中，删除失败"),
    CANNOT_DELETE_SUPER(0, "不能删除超级管理员"),
    NOT_FOUND_HEAD_URL(0, "头像地址为空"),
    KEY_WORD_NOT_FOUND(0, "关键词不能为空"),
    NOT_FOUND_USERINFO(401, "获取用户信息失败"),
    /**
     * 学习指南相关的code
     */
    STUDY_GUIDE_CREATE(1, "学习指南创建成功"),
    STUDY_GUIDE_TYPE(0, "学习指南类型错误"),
    BACKSTAGE_EXCEPTION(0, "后台异常"),
    STUDY_GUIDE_LIST_NOT(0, "学习指南列表获取失败"),
    STUDY_GUIDE_NOT_ENTERPRISE(0, "该学习指南不属于该企业"),
    /**
     * app相关的
     */
    USER_ID_APP_ID_EMPTY(-4, "用户id和应用id不能为空"),

    TEST_ACCOUNT_CANNOT_LOGIN(-2, "测试账号暂不允许登陆"),
    ACCOUNT_NOT_FOUND(-2, "账号未注册，请先注册"),
    LOGIN_FAIL(-2, "账号或密码错误,登录失败"),
    OLD_PASSWORD_SAME_NEW(0, "密码不能与旧密码相同"),
    OLD_PASSWORD_FAIL(0, "旧密码不正确，请重新输入"),
    USER_NOT_IN_ENTERPRISE(0, "用户不在当前企业中，无法切换企业"),
    NOT_FOUND_USER_ROLE(0, "数据异常，未获得用户角色信息"),
    AUDIT_TYPE_ERROR(0, "审核类型错误"),
    AUDIT_COMMENT_ERROR(0, "审核信息为空"),
    UPDATA_FAIL(0, "审核失败"),
    TRADE_ERROR(0, "解析贸易平台返回接口错误，调用失败"),
    LEARN_TYPE_ERROR(0, "学习指南类型错误"),
    INSERT_LEARN_FAILED(0, "学习指南新增失败"),
    UPDATE_LEARN_FAILED(0, "学习指南修改失败"),
    DELETE_LEARN_FAILED(0, "学习指南删除失败"),
    LEARN_NOT_FOUND(0, "没有找到相关学习指南"),

    /**
     * 接口相关
     */
    INTERFACE_NOT_EXIST(0, "接口不存在"),
    INTERFACE_ID_EMPTY(0, "接口id不能为空"),
    INTERFACE_PARAM_EMPTY(0, "接口参数不能为空"),
    INTERFACE_EXISTS(0, "数据已存在"),

    /**
     * 平台用户相关
     */
    PLATFORM_USER_ID_EMPTY(0, "用户id不能为空"),
    PLATFORM_USER_NOT_EXIST(0, "平台用户不存在"),
    UPDATE_PLATFORM_USER_STATUS(0, "修改平台用户状态失败"),
    PLATFORM_USER_DISABLE(0, "用户已被禁用"),
    EMAIL_NOT_RESET_PASSWORD(0, "该用户邮箱为空，不能使用邮箱重置密码"),

    /**
     * 后台添加客户
     */
    PLATFORM_USER_AT_ALREADY(0, "该用户已注册"),
    USER_EXISTS(0, "该用户已经存在,请勿重复添加"),
    PLATFORM_USER_NOT(0, "没有不存在"),
    OLD_PHONE_SAME_NEW(0, "手机号不能与旧手机号相同"),
    OLD_EMAIL_SAME_NEW(0, "邮箱不能与旧邮箱相同"),
    EMAIL_REPETITION(-1, "该邮箱账户已被使用"),
    PASSWORD_RESET_FAIL(0, "密码重置失败"),
    IMPORT_INTERFACE_FAIL(0, "excel批量导入失败，数据有误"),
    /**
     * 数据驾驶相关
     */
    RECORD_NOT_EXIST(0, "记录不存在"),
    CUSTOMER_NOT_EXIST(0, "客户不存在"),
    CUSTOMER_CATEGORY_NOT_EXIST(0, "客户分类不存在"),
    PRODUCT_NOT_EXIST(0, "产品不存在"),
    PRODUCT_IS_USED(0, "产品已被使用"),
    PRODUCT_IS_USED_BATCH_NOT_UPDATE(0, "产品已被使用，不能修改批次"),
    PRODUCT_CATEGORY_NOT_EXIST(0, "产品分类不存在"),
    FURNACE_NOT_EXIST(0, "炉号不存在"),
    FURNACE_IS_USED(0, "炉号已使用，不能删除"),
    CONTACT_NOT_EXIST(0, "联系人不存在"),
    PRODUCT_NAME_EXIST(0, "产品名称已存在"),
    FURNACE_NAME_EXIST(0, "设备名称已存在"),
    PRODUCT_CATEGORY_NAME_EXIST(0, "产品分类名称已存在"),
    CODE_EXIST(0, "编码已存在"),
    PURCHASE_GOODS_ORDER_NOT_EXIST(0, "采购收货单不存在"),
    PURCHASE_GOODS_ORDER_ID_EMPTY(0, "采购收货单id不能为空"),
    FEEDBACK_NOT_FIND(0, "未找到相关意见反馈"),
    ENTERPRISE_NOT_FIND(0, "未找到对应企业"),
    NAME_EXIST(0, "名称已存在"),
    PURCHASE_GOODS_ORDER_CODE_ERROR(0, "入库单号格式错误"),
    PURCHASE_GOODS_ORDER_CODE_EXIST(0, "入库单号已存在"),
    PURCHASE_STATEMENT_ORDER_CODE_ERROR(0, "结算单号格式错误"),
    PURCHASE_STATEMENT_ORDER_CODE_EXIST(0, "结算单号已存在"),
    PURCHASE_STATEMENT_ORDER_NOT_EXIST(0, "结算单不存在"),
    PURCHASE_STATEMENT_ORDER_ID_EMPTY(0, "结算单id不能为空"),
    STATEMENT_DELIVERY_CODE_ERROR(0, "出库单号格式错误"),
    STATEMENT_DELIVERY_CODE_EXIST(0, "出库单号已存在"),
    STATEMENT_DELIVERY_ORDER_NOT_EXIST(0, "销售发货单不存在"),
    STATEMENT_DELIVERY_ORDER_ID_EMPTY(0, "销售发货单id不能为空"),
    CATEGORY_EXIST(0, "分类名称已存在"),
    SALE_STATEMENT_ORDER_CODE_ERROR(0, "销售结算单号格式错误"),
    SALE_STATEMENT_ORDER_CODE_EXIST(0, "结算单号已存在"),
    SALE_STATEMENT_ORDER_NOT_EXIST(0, "销售结算单不存在"),
    SALE_STATEMENT_ORDER_ID_EMPTY(0, "销售结算单id不能为空"),
    SALE_ORDER_CODE_ERROR(0, "销售订单号格式错误"),
    SALE_ORDER_CODE_EXIST(0, "销售订单号已存在"),
    FAILED_GET_FIRST_CUSTOMER_CATEGORY(0, "获取客户一级分类失败"),
    REPOSITORY_NO(0, "没查到仓库档案"),
    REPOSITORY_NAME_EXIST(0, "仓库名称已存在"),
    REPOSITORY_IS_USED(0, "仓库已使用，不能删除"),
    PRODUCT_NOT_UNIQUE(0, "一个订单下只能选择一种产品"),

    /**
     * 后台相关
     **/
    NOT_FOUND_PLATFORM_MANAGE(-1, "该手机号尚未开通后台账户"),
    NOT_FOUND_PLATFORM_USER(-1, "该手机号尚未开通中台账户"),
    PROHIBIT_ACCOUNT(-1, "本账号已被禁用，请联系管理员处理。"),
    ACCOUNT_REPETITION(-1, "账号已存在请勿重复添加"),
    UNVERIFIED(-2, "登录校验失败"),
    ROLE_ID_NOT_NULL(-1, "权限组不能为空"),
    MANAGER_ROLE_NOT_DELETE(-1, "角色组不存在或不能被删除"),
    CAN_NOT_DELETE_MANAGER_ROLE(0, "权限组“{0}”下账户数量为{1},不可删除！"),
    MANAGER_ROLE_NOT_FOUND(-1, "权限组不存在"),
    ROLE_ID_ERROR(-1, "角色id有误，无法查询到对应角色"),
    MANAGER_ROLE_GROUP_EXIT(-1, "权限组名称重复"),
    PHONE_ALREADY_EXIST(0, "“手机号”和已有账户冲突！"),
    NAME_ALREADY_EXIST(0, "“姓名”和已有账户冲突！"),
    EMAIL_ALREADY_EXIST(0, "“邮箱”和已有账户冲突！"),
    MANAGER_UNABLE_FORBIDDEN(0, "企业管理员无法禁用,请更换身份后再做操作"),
    DEPARTMENT_NOT_TO_THIS(0, "当前企业下没有该部门"),
    POST_NOT_TO_THIS(0, "当前部门下没有该职位"),
    ACCOUNT_NAME_REPEAT(0, "账户名重复"),
    ADMIN_NOT_FOUND(0, "企业管理员不存在"),
    NOT_PROHIBIT_YOURSELF(0, "无法禁用自己"),
    WORK_ORDER_HAS_CLOSED(-1, "工单已关闭，不能再次操作"),
    NOT_AUTN_ACCESS(0, "权限验证失败,用户无权访问"),
    AUTHORITY_CONFIGURATION_FAILED(-1, "权限配置失败"),

    /**
     * 中台相关
     **/
    SEND_SMS_FAILED(0, "短信推送失败！"),
    APP_NOT_FIND(0, "未找到相应产品"),
    APP_ROLE_NOT_FIND(0, "未找到相应产品角色"),
    ERROR_EMAIL_CODE(0, "邮箱验证码错误"),
    DEFAULT_USERGROUP_EXIST(0, "已存在默认用户组"),
    USERGROUP_IS_USERD(0, "用户组在使用，无法删除"),
    USERGROUP_NOT_EXIST(0, "用户组不存在"),


    /**
     * 成本小工具相关
     */

    ERP_COST_ALLOCATION_NOT_FOUND(0, "该配置不存在"),
    ERP_COST_ALLOCATION_DENY_FORBIDDEN(0, "该配置正在使用中，不能禁用"),
    ERP_COST_ALLOCATION_DENY_UPDATE(0, "该配置正在使用中，不能修改"),
    ERP_COST_ALLOCATION_DENY_DELETE(0, "该配置正在使用中，不能删除"),
    ERP_COST_SIMULATION_NOT_FOUND(0, "该成本模拟不存在"),
    ERP_COST_SIMULATION_RESULT_EXIT(0, "已存在已确认的数据"),
    ERP_COST_SIMULATION_RESULT_ONLY(0, "计算结果中只能确认一条数据"),
    SI_C_CONTENT_TOO_BIG(0,"硅和碳含量之和要小于100"),
    COKERATIO_SEMICOKERATIO_ADD_IS_ONE(0,"焦炭比例和兰炭比例之和为100%"),
    
	
	
	/**
	 * 物流相关的
	 */
	LOGISTICS_ORDER_NUMBER_NOT(0, "物流订单号码已存在"),
	ORDER_NUMBER_NOT(0,"物流订单号不能为空"),
	ORDER_ID_NAME_REPEAT(0, "物流id重复"),
	INVOICE_NUMBER_NOT(0,"物流发票单号重复"),
	PAY_ORDER_NO_NOT(0, "物流付款单号重复"),


    /**
     * 砂石相关
     */
    CUSTOMER_NOT_FORBIDDEN(0, "客户未停用，不能删除"),
    NO_SELECT_RECORD(0,"未选择记录"),
    SALE_ORDER_ID_EMPTY(0, "销售合同id不能为空"),
    SALE_ORDER_EXIST_PAYMENT(0, "不能删除已回款的销售合同"),
    SALE_ORDER_EXIST_SETTLEMENT(0, "不能删除已结算的销售合同"),
    SALE_ORDER_EXIST_INSPECTION_GOODS(0, "不能删除已验货的销售合同"),
	DINAS_PURCHASE_PAY_NOT_ID(0, "采购付款id错误"),
	DINAS_PURCHASE_INVOICE_NOT_ID(0, "采购发票id错误"),
	DINAS_SALE_ORDER_PRODUCT_REPEAT(0, "产品和规格重复"),
	DINAS_SALE_ORDER_EXIST(0, "销售合同号已存在"),
	DINAS_SALE_PAYMENT_EXIST(0, "销售回款单号已存在");

    public Integer code;

    public String msg;

}
