package com.bee.platform.common.exception;

import lombok.AllArgsConstructor;
import lombok.Getter;


/**
 * @author zhigang.zhou
 * @version 1.0.0
 * @ClassName ExceptionMessageEnum
 * @Description 所有参照业务模块的com.bee.trade.common.entity.ResCodeEnum的值进行 {@code code} 定义相应的错误编码！
 * 每个业务模块的错误使用相应的错误编码区间！详情见示例！
 * @Date 2018年10月26日 下午4:00:47
 */
@Getter
@AllArgsConstructor
public enum ExceptionMessageEnum {

    /**
     * 系统级错误异常
     */
    SUCCESS(0, "成功"),
    SYSTEM_INVALID_PARAMS(1, "参数错误"),
    SYSTEM_NOT_FOUND(10, "无法找到相应的数据"),
    ERROR_SYSTEM(-1, "系统繁忙,请稍后再试"),
    ERROR_DATA(-1, "无法找到相应数据"),

    /**
     * 示例异常：用户模块
     */
    USER_DATABASE_OPERATE_FAILED(20001, "操作失败"),
    USER_VALUE_DUPLICATE(20002, "name 不允许重复"),
    USER_VALUE_EMPTY(20003, "参数不允许为空"),
    USER_VALUE_OUT_RANGE(20004, "参数不是限定枚举值"),
    USER_NOT_LEGAL(20005, "登录凭证有误，请重新登录"),
    USER_CAN_NOT_UPDATE(20006, "菜单有下级菜单，其类型不能改变"),
    USER_METHOD_ERROR(20007, "无菜单时添加菜单，不允许添加子菜单或子页面"),
    USER_CAN_NOT_HAVE_URL(20008, "菜单类型为普通菜单时，页面路由不允许有值"),
    USER_MENUID_NOT_LEGAL(20009, "参数格式不合法"),
    USER_NO_SUCH_VALUE(20010, "没有找到值"),
    NO_AUTHORITY(20011, "没有权限"),
    USER_NO_MENU(20012, "用户没有分配权限（菜单）"),
    USER_VALUE_NOT_LEGAL(20013, "参数值位数或类型错误"),
    USER_LOGIN_USER_INFO_EMPTY(200014, "获取当前登录用户公司id，公司名称失败"),
    USER_CAN_NOT_HAVE_CHILDID(20015, "url菜单不允许有下级菜单"),
    USER_JSON_ANALYSIS_FAILED(20016, "json解析数组失败"),
    USER_NO_PARENT(20016, "url菜单必须至少有一级上级普通菜单"),
    CHANGE_COMPANY_FAIL(20017, "切换企业失败"),
    MUST_BEETRADE_COMPANY(20018, "必须是{0}登录的用户才可进入{1}"),
    COMPANY_CURBED_ROLE(20019, "必须是{0}的用户才能设置为{1}"),
    PERMISSION_NOT_FOUND(20020, "权限不存在"),
    CAN_NOT_EDIT_ROLE(20021, "企业不存在，不能为该角色限制可访问企业"),
    NOT_CHANGE_ROLE(20022, "系统自动为用户分配{0}角色，请勿手动修改"),
    FAILED_TO_GET_USER_INFO(401, "登录信息有误，请重新登录"),
    NOT_CHOICE_ENTERPRISE(20024, "未选择企业"),
    USER_NOT_AUTHORIZE(20026,"用户未授权,请与管理员联系"),
    USER_MOBILE_EXIST(20030, "手机号已被使用"),
    SYSTOKEN_OVERDUE(20025, "登录信息已过期，请重新登录"),

    USER_INVALID_GTICKET(20031, "请先登录"),
    USER_INVALID_EMPLOYEE_NAME(200032, "员工姓名格式不正确"),
    USER_INVALID_MOBILE(200033, "请输入正确的手机号"),
    USER_INVALID_EMP_NUM(200034, "员工编码格式不正确"),
    USER_INVALID_EMP_POSITION(200035, "导入职位格式不正确"),
    USER_INVALID_ORGANIZAITION(200036, "组织架构格式不正确"),
    USER_INVALID_AVAILABLE(200037, "不是限定的员工状态"),
    USER_MOBILE_EXIT(200038, "手机号已存在"),
    USER_INVALID_EXCEL(200039, "excel文件名不正确"),
    USER_NO_SUCH_ROLE(200041, "输入了未知的角色"),
    USER_NEED_NECESSARY_PARAM(200042, "缺少必要的参数"),
    USER_INVALID_JSON_PARAM(200043, "不合法的json字符串"),
    USER_MOBILE_LIMIT_TIME(200044, "该手机号未过限定时间"),
    USER_SMS_FAILD(200045, "获取验证码失败"),
    USER_CHECK_CODE_NOT_EXIT(200046, "验证码不存在"),

    USER_SMS_NOT_CHECKED(200047, "没有经过短信验证"),
    USER_NO_VISITOR_MESSAGE(200048, "没有访问者系统的信息"),
    USER_NOT_SUPPORTED_EMP_TYPE(200049, "不支持添加其他类员工"),
    USER_INVALID_USERNAME_PASSWORD(200050, "账号或密码错误,请重新输入！"),
    USER_INVALID_BIRTHDAY(200051, "此人尚未出生"),
    USER_INVALID_MARKET_CODE(200052, "商场编码格式不正确"),
    USER_INVALID_MARKET_NAME(200053, "商场名称格式不正确"),
    USER_INVALID_MARKET_CODE_NAME(200054, "商场编码与名称不匹配"),
    USER_EXCEL_NO_DATAS(200055, "请导入有数据的excel"),
    USER_EMPTY_ADDRESS(200056, "地址不能为空"),
    USER_INVALID_PARAM(200057, "格式不正确，请重新输入"),
    USER_WRONG_PASSWORD(200058, "原始密码错误，请重新输入"),
    USER_NOT_ALLOWED_MODIFICATION(200059, "此处不允许修改自己的信息"),
    PRODUCT_DELETE(200060, "成功删除{0}件商品，失败{1}件，其中{2}件商品不存在或已被删除"),
    PRODUCT_SHELVES(200061, "成功下架{0}件商品，失败{1}件，其中{2}件商品不存在或已下架"),
    PRODUCT_ON_SHELF(200062, "成功上架{0}件商品，失败{1}件，其中{2}件商品不存在或已上架"),
    PROTOCOL_FAIL(200063, "该补充协议双方已确认，无法编辑"),
    PROTOCOL_NOT_EXIST(200064, "该协议不存在"),
    CONTRACT_NOT_FOUND(200065, "该合同不存在"),
    PROTOCOL_EDIT_FAIL(200066, "该协议已方已经确认，请等待对方确认"),
    CONTRACT_NOT_MUTUAL_CONFIRM(200067, "该补充协议双方已确认，无法编辑"),
    CONTRACT_NOT_ALL_COMFIRM(200068, "合同未双方确认，不允许生成协议"),
    REPEATED_SUBMIT(200069, "重复提交"),
    VALIDATE_CODE_EXPIRE(200070, "验证码已过期，请重新发送"),
    INSERT_SYSTEM_NOTICE_FAILED(200071, "新增系统通知信息失败"),
    VALIDATE_CODE_ERROR(200072, "验证码错误"),
    /**
     * 文章相关
     */
    DELETE_ARTICLE_FAILED(200100, "文章删除失败"),
    UPDATE_ARTICLE_FAILED(200101, "文章编辑失败"),
    INSERT_ARTICLE_FAILED(200102, "文章新增失败"),
    ARTICLE_TYPE_ERROR(200103, "文章类型错误"),
    ARTICLE_NOT_FOUND(200103, "公告不存在"),
    ARTICLE_TYPE_NOT_EMPTY(200103, "该类型下存在公告，无法删除"),
    /**
     * app相关
     */
    UPDATE_APP_ID_FAILED(200104, "更新appID失败"),
    USER_OPEN_APP_FAILED(200105, "用户开通应用失败"),
    USER_REMOVE_APP_FAILED(200106, "关闭应用失败"),
    USER_ID_PARAMS_NOT_FOUND(200107, "请求参数异常，请求的USER_ID未匹配到相关企业用户信息"),
    APP_ID_PARAMS_NOT_FOUND(200108, "请求参数异常，请求的APP_ID未匹配到相关应用信息"),
    USER_ID_APP_ID_IS_EMPTY(200109, "请求参数异常，请求的USER_ID或APP_ID为空"),
    ENTERPRISESAPPSLOG_SAVE_FAILED(0, "日志记录保存异常"),

    /**
     * 学习指南相关
     */
    INSERT_LEARN_FAILED(200110, "学习指南新增失败"),
    UPDATE_LEARN_FAILED(200111, "学习指南修改失败"),
    DELETE_LEARN_FAILED(200112, "学习指南删除失败"),
    LEARN_NOT_FOUND(200113, "没有找到相关学习指南"),
    LEARN_TYPE_ERROR(200114, "学习指南类型错误"),
    STUDY_GUIDE_NOT_ENTERPRISE(200115, "该学习指南不属于该企业"),

    /**
     * 企业相关
     */
    ENTERPRISE_SAVE_FAILED(0, "企业信息保存异常"),
    DEPARTMENT_FIND_FAILED(0, "部门信息查询失败"),
    DEPARTMENT_FIRST_FIND_FAILED(0, "父级部门信息查询失败"),
    DEPARTMENT_SAVE_FAILED(0, "部门信息保存异常"),
    DEPARTMENT_NAME_EXISTED(0, "部门名重复，请重新编辑"),
    DEPARTMENT_LEVEL_ERROR(0, "新增部门层级数不得小于等于上级部门"),
    POST_NAME_EXISTED(0, "职位名重复，请重新编辑"),
    POST_SAVE_FAILED(0, "职位信息保存异常"),
    POST_DELETE_FAILED(0, "职位信息删除异常"),
    ENTERPRISE_HEAD_UPDATE_FAILED(0, "企业头像信息修改失败"),
    DEPARTMENT_DELETE_FAILED(0, "部门删除失败"),
    DEPARTMENT_SUB_DELETE_FAILED(0, "子部门删除失败"),
    ENTERPRISE_DELETE_USER_FAILED(0, "公司移除用户失败"),
    ENTERPRISE_CHECK_UPDATE_FAILED(0, "企业申请信息修改失败"),
    ENTERPRISE_CHECK_SAVE_FAILED(0, "企业申请信息保存失败"),
    ENTERPRISE_ASSOCIATED_SAVE_FAILED(0, "企业关联用户信息保存失败"),
    ENTERPRISE_ASSOCIATED_UPDATE_FAILED(0, "企业关联用户信息修改失败"),
    ENTERPRISE_EXIST(0, "企业已注册"),
    ENTERPRISE_REPEAT(0, "企业名称异常，企业重复"),
    ENTERPRISE_IN_AUDIT(0, "企业正在审核中"),
    ENTERPRISE_NOT_EXIST(201001, "企业不存在"),
    USER_NOT_IN_ENTERPRISE(201002, "当前企业无该用户"),
    USER_IN_ENTERPRISE(201003, "当前企业已关联，不可再次申请关联"),
    USERNAME_NOT_FOUND(201004, "用户账号不存在"),
    ENTERPRISE_CHECK_NOT_EXIST(201005, "企业认证单不存在"),
    ENTERPRISE_AUDIT_FAILED(201006, "企业审核失败"),
    ENTERPRISE_UPDATE_AUDIT_FAILED(201007, "企业修改审核失败"),
    SEND_INVITE_SUCCESS(201008, "邀请发送成功"),
    SEND_INVITE_FAILED(201009, "邀请发送失败"),
    SEND_INVITE_ALREADY(2010010, "已邀请过，零点后重试"),
    ENTERPRISE_ID_NOT_EXIST(2010011, "企业ID不存在"),
    ENTERPRISE_ATTACHE_UPDATE_FAILED(2010012, "企业附件修改失败"),
    DEPARTMENT_NOT_TO_THIS(0, "当前企业下没有该部门 请重新编辑"),
    POST_NOT_TO_THIS(0, "当前部门下没有该职位"),
    ADMIN_NOT_FOUND(201004, "企业管理员不存在"),
    CANCEL_FAIL(201005, "企业认证取消失败"),

    /**
     * 用户相关
     */
    USER_SAVE_FAILED(0, "用户信息保存异常"),
    USER_UPDATE_FAILED(0, "用户信息更新异常"),
    USER_ROLE_RELATION_SAVE_FAILED(0, "用户角色信息保存失败"),
    USER_ENTERPRISE_RELATION_SAVE_FAILED(0, "企业用户信息保存失败"),
    USER_ENTERPRISE_RELATION_UPDATE_FAILED(0, "企业用户信息更新失败"),
    USER_ENTERPRISE_RELATION_FIND_FAILED(0, "企业用户信息查询失败"),
    USER_ENTERPRISE_NOT_FOUND(0, "未找到企业用户信息"),
    USER_DEPARTMENT_RELATION_SAVE_FAILED(0, "部门用户信息保存失败"),
    USER_DEPARTMENT_RELATION_UPDATE_FAILED(0, "部门用户信息更新失败"),
    USER_ROLR_SAVE_FAILED(0, "用户角色信息保存异常"),
    USER_ROLR_UPDATE_FAILED(0, "用户角色信息更新异常"),
    USER_NOT_EXIST(202001, "用户不存在"),
    USER_ALREADY_ADMIN(202002, "用户已经是管理员"),
    USER_NOT_ADMIN(202003, "用户不是管理员"),
    MANAGER_ROLE_GROUP_EXIT(202004, "权限组名称重复"),
    ACCOUNT_REPETITION(202005, "账号已存在请勿重复添加"),
    EMAIL_ERROR(202006, "请输入正确的邮箱地址"),
    EMAIL_REPETITION(202007, "该邮箱账户已被使用"),
    ACCOUNT_NAME_REPEAT(202008, "账户名重复"),
    NOT_VALIDATE(202009, "验证码未成功验证"),
    OLD_PASSWORD_SAME_NEW(202010, "密码不能与旧密码相同"),
    EMAIL_TEMPLATE_NOT_FOUND(202011, "邮件类型模板不存在"),
    NOT_FOUND_USER_ROLE(202012, "数据异常，未获得用户角色信息"),

    /**
     * 公共模块
     */
    TRADE_GOODS_GRADE_NOT_FOUND(2400001, "标的物{0}的低分规则配置不存在"),
    /**
     * 角色功能应用相关
     */
    FUNCTION_ROLE_SAVE_FAILED(0, "角色功能应用信息保存失败"),
    FUNCTION_ROLE_UPDATE_FAILED(0, "角色功能应用信息修改失败"),
    FUNCTION_ROLE_DELETE_FAILED(0, "角色功能应用信息删除失败"),
    USER_ROLE_SAVE_FAILED(0, "用户与角色功能应用关联信息保存失败"),
    USER_ROLE_BACK_SAVE_FAILED(0, "后台用户与角色功能应用关联信息保存失败"),
    USERGROUP_ROLE_SAVE_FAILED(0, "用户组与角色功能应用关联信息保存失败"),
    USERGROUP_ROLE_BACK_SAVE_FAILED(0, "后台用户组与角色功能应用关联信息保存失败"),
    USERGROUP_USER_SAVE_FAILED(0, "用户组与用户关联信息保存失败"),
    USER_ROLE_SAVE_FAILED_NO_USER_ID(0, "用户与角色功能应用关联信息保存失败，用户id为空"),
    USER_ROLE_BACK_SAVE_FAILED_NO_USER_ID(0, "后台用户与角色功能应用关联信息保存失败，用户id为空"),
    USERGROUP_ROLE_SAVE_FAILED_NO_USERGROUP_ID(0, "用户组与角色功能应用关联信息保存失败，用户组id为空"),
    USERGROUP_ROLE_BACK_SAVE_FAILED_NO_USERGROUP_ID(0, "后台用户组与角色功能应用关联信息保存失败，后台用户组id为空"),
    USER_ROLE_SAVE_FAILED_DELETED_OLD_FAILED(0, "用户与角色功能应用关联信息保存失败,删除旧数据失败"),
    USER_ROLE_BACK_SAVE_FAILED_DELETED_OLD_FAILED(0, "后台用户与角色功能应用关联信息保存失败,删除旧数据失败"),
    USERGROUP_ROLE_SAVE_FAILED_DELETED_OLD_FAILED(0, "用户组与角色功能应用关联信息保存失败,删除旧数据失败"),
    USERGROUP_ROLE_BACK_SAVE_FAILED_DELETED_OLD_FAILED(0, "后台用户组与角色功能应用关联信息保存失败,删除旧数据失败"),
    USERGROUP_USER_SAVE_FAILED_DELETED_OLD_FAILED(0, "用户组与用户关联信息保存失败,删除旧数据失败"),
    USER_ROLE_SAVE_FAILED_ERROR_P(0, "用户与角色功能应用关联信息保存失败,请求参数不全-必须包含用户id、企业id、角色id、pid、roleType"),
    USER_ROLE_BACK_SAVE_FAILED_ERROR_P(0, "后台用户与角色功能应用关联信息保存失败,请求参数不全-必须包含用户id、角色id、pid、roleType"),
    USERGROUP_ROLE_SAVE_FAILED_ERROR_P(0, "用户组与角色功能应用关联信息保存失败,请求参数不全-必须包含用户id、企业id、角色id、pid、roleType、subSys"),
    USERGROUP_ROLE_BACK_SAVE_FAILED_ERROR_P(0, "后台用户组与角色功能应用关联信息保存失败,请求参数不全-必须包含用户id、企业id、角色id、pid、roleType、subSys"),
    USER_ROLE_UPDATE_FAILED(0, "用户与角色功能应用关联信息修改失败"),
    USER_ROLE_DELETE_FAILED(0, "用户与角色功能应用关联信息删除失败"),
    CUSTOMER_ROLE_SAVE_FAILED(0, "客户与角色功能应用关联信息保存失败"),
    CUSTOMER_ROLE_UPDATE_FAILED(0, "客户与角色功能应用关联信息修改失败"),
    CUSTOMER_ROLE_DELETE_FAILED(0, "客户与角色功能应用关联信息删除失败"),
    TEST_ACCOUNT_CANNOT_LOGIN(-2, "测试账号暂不允许登陆"),
    ACCOUNT_NOT_FOUND(-2, "账号未注册，请先注册"),
    PLATFORM_USER_DISABLE(-3, "用户已被禁止登录"),
    AUTHORITY_CONFIGURATION_FAILED(-1, "权限配置失败"),

    /**
     * erp采购相关
     */
    ERP_PURCHASE_PRODUCT_REPEAT(0, "erp采购订单只能对应一种产品"),
    ERP_PURCHASE_ADD_FAILED(0, "erp采购订单添加失败"),
    ERP_PURCHASE_UPDATE_FAILED(0, "erp采购订单更新失败"),
    ERP_PURCHASE_DELETE_FAILED(0, "erp采购订单删除失败"),
    ERP_CODE_REPEAT(0, "单据编号重复，请重新输入"),
    ERP_INVOICE_STATE_UPDATE_FAILED(0, "修改订单开票状态失败"),
    /**
     * erp采购明细相关
     */
    ERP_PURCHASE_DETAIL_ADD_FAILED(0, "erp采购订单明细添加失败"),
    ERP_PURCHASE_DETAIL_UPDATE_FAILED(0, "erp采购订单明细更新失败"),
    ERP_PURCHASE_DETAIL_DELETE_FAILED(0, "erp采购订单明细删除失败"),

    /**
     * erp化验单相关
     */
    ERP_TEST_REPORT_REPEAT(0, "erp化验单编号重复"),
    ERP_TEST_REPORT_ADD_FAILED(0, "erp化验单添加失败"),
    ERP_TEST_REPORT_UPDATE_FAILED(0, "erp化验单更新失败"),
    ERP_TEST_REPORT_DETAIL_DELETE_FAILED(0, "erp化验单删除失败"),
    ERP_TEST_REPORT_MAIN_VALUE_EMPTY(0, "主属性不能为空"),
    ERP_TEST_REPORT_LOG_ADD_FAILED(0, "erp化验单日志添加失败"),
    /**
     * erp化验单类型相关
     */
    ERP_TEST_TYPE_ADD_FAILED(0, "erp化验单类型添加失败"),
    ERP_TEST_TYPE_UPDATE_FAILED(0, "erp化验单类型更新失败"),
    ERP_TEST_TYPE_DELETE_FAILED(0, "erp化验单类型删除失败"),
    USED_TEST_TYPE_CANNOT_DELETE(0, "已引用化验单类型不能删除"),
    /**
     * 采购收票
     */
    ERP_PURCHASE_INVOICE_ADD_FAILED(0, "erp化验单类型添加失败"),
    ERP_PURCHASE_INVOICE_UPDATE_FAILED(0, "erp化验单类型更新失败"),
    ERP_PURCHASE_INVOICE_DELETE_FAILED(0, "erp化验单类型删除失败"),
    /**
     * 采购收票明细
     */
    ERP_PURCHASE_INVOICE_DETAIL_ADD_FAILED(0, "erp化验单类型添加失败"),
    ERP_PURCHASE_INVOICE_DETAIL_UPDATE_FAILED(0, "erp化验单类型更新失败"),
    ERP_PURCHASE_INVOICE_DETAIL_DELETE_FAILED(0, "erp化验单类型删除失败"),
    /**
     * 销售收票
     */
    ERP_SALE_INVOICE_ADD_FAILED(0, "erp化验单类型添加失败"),
    ERP_SALE_INVOICE_UPDATE_FAILED(0, "erp化验单类型更新失败"),
    ERP_SALE_INVOICE_DELETE_FAILED(0, "erp化验单类型删除失败"),
    /**
     * 销售收票明细
     */
    ERP_SALE_INVOICE_DETAIL_ADD_FAILED(0, "erp化验单类型添加失败"),
    ERP_SALE_INVOICE_DETAIL_UPDATE_FAILED(0, "erp化验单类型更新失败"),
    ERP_SALE_INVOICE_DETAIL_DELETE_FAILED(0, "erp化验单类型删除失败"),
    /**
     * 采购付款单
     */
    ERP_PAY_ADD_FAILED(0, "erp采购付款单添加失败"),
    ERP_PAY_UPDATE_FAILED(0, "erp采购付款单更新失败"),
    ERP_PAY_DELETE_FAILED(0, "erp采购付款单删除失败"),
    /**
     * erp销售收款相关
     */
    ERP_RECEIPT_ORDER_SAVE_FAILED(0, "erp销售收款单保存失败"),
    ERP_RECEIPT_ORDER_SAVE_FAILED_NO_DETAIL(0, "erp销售收款单保存失败,没有详情信息"),
    ERP_RECEIPT_ORDER_SAVE_FAILED_NO_DETAIL_MONEY(0, "erp销售收款单保存失败,没有详情单本次收款金额"),
    ERP_RECEIPT_ORDER_SAVE_FAILED_CODE_RE(0, "erp销售收款单保存失败,收款单号编码重复"),
    ERP_RECEIPT_ORDER_SAVE_FAILED_MONEY_D(0, "erp销售收款单保存失败,总金额与明细金额不一致"),
    ERP_RECEIPT_ORDER_UPDATE_FAILED(0, "erp销售收款单修改失败"),
    ERP_RECEIPT_ORDER_UPDATE_FAILED_MONEY_D(0, "erp销售收款单修改失败,总金额与明细金额不一致"),
    ERP_RECEIPT_ORDER_DELETE_FAILED(0, "erp销售收款单删除失败"),
    ERP_RECEIPT_ORDER_DETAIL_SAVE_FAILED(0, "erp销售收款单保存失败"),
    ERP_RECEIPT_ORDER_DETAIL_UPDATE_FAILED(0, "erp销售收款单修改失败"),
    ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED(0, "erp销售收款单删除失败"),
    ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_UPDATE_SALE_ORDER_FAILED(0, "erp销售收款单删除失败,修改销售订单累计收款金额失败"),
    ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_NO_DATA(0, "erp销售收款单删除失败,没有此详情单"),
    ERP_RECEIPT_ORDER_DETAIL_DELETE_FAILED_NO_SALE_ORDER(0, "erp销售收款单删除失败,没有此销售订单"),
    /**
     * erp料批相关
     */
    ERP_MATERIAL_BATCH_ORDER_SAVE_FAILED(0, "erp料批单保存失败"),
    ERP_MATERIAL_BATCH_ORDER_UPDATE_FAILED(0, "erp料批单修改失败"),
    ERP_MATERIAL_BATCH_ORDER_DELETE_FAILED(0, "erp料批单删除失败"),
    ERP_MATERIAL_BATCH_ORDER_DETAIL_SAVE_FAILED(0, "erp料批详情单保存失败"),
    ERP_MATERIAL_BATCH_ORDER_DETAIL_UPDATE_FAILED(0, "erp料批详情单修改失败"),
    ERP_MATERIAL_BATCH_ORDER_DETAIL_DELETE_FAILED(0, "erp料批详情单删除失败"),
    /**
     * erp库存盘点相关
     */
    ERP_STOCK_CHECK_SAVE_FAILED(0, "erp库存盘点保存失败"),
    ERP_STOCK_CHECK_UPDATE_FAILED(0, "erp库存盘点修改失败"),
    ERP_STOCK_CHECK_DELETE_FAILED(0, "erp库存盘点删除失败"),
    ERP_STOCK_CHECK_DETAIL_SAVE_FAILED(0, "erp库存盘点详情保存失败"),
    ERP_STOCK_CHECK_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED(0, "erp库存盘点详情保存失败，调整库存失败"),
    ERP_STOCK_CHECK_DETAIL_SAVE_FAILED_NO_STOCK(0, "erp库存盘点详情保存失败,库存表里没有该产品"),
    ERP_STOCK_CHECK_DETAIL_UPDATE_FAILED(0, "erp库存盘点详情修改失败"),
    ERP_STOCK_CHECK_DETAIL_DELETE_FAILED(0, "erp库存盘点详情删除失败"),
    ERP_STOCK_CHECK_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED(0, "erp库存盘点详情删除失败,调整库存失败"),


    /**
     * erp辅材消耗相关
     */
    ERP_AUXILIARY_MATERIAL_CONSUMPTION_SAVE_FAILED(0, "erp辅材消耗保存失败"),
    ERP_AUXILIARY_MATERIAL_CONSUMPTION_SAVE_FAILED_NO_MONEY(0, "erp辅材消耗保存失败，四个金额至少填一个"),
    ERP_AUXILIARY_MATERIAL_CONSUMPTION_UPDATE_FAILED(0, "erp辅材消耗修改失败"),
    ERP_AUXILIARY_MATERIAL_CONSUMPTION_DELETE_FAILED(0, "erp辅材消耗删除失败"),

    /**
     * erp领料出库相关
     */
    ERP_OUT_OF_STOCK_ORDER_SAVE_FAILED(0, "erp领料出库保存失败"),
    ERP_OUT_OF_STOCK_ORDER_SAVE_FAILED_CODE_RE(0, "erp领料出库保存失败,编号重复"),
    ERP_OUT_OF_STOCK_ORDER_UPDATE_FAILED(0, "erp领料出库修改失败"),
    ERP_OUT_OF_STOCK_ORDER_DELETE_FAILED(0, "erp领料出库删除失败"),
    ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED(0, "erp领料出库详情保存失败"),
    ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED_NO_P(0, "erp领料出库详情保存失败,库存表里没有该产品，不能出库不存在的产品"),
    ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED(0, "erp领料出库详情保存失败,调整库存数量失败"),
    ERP_OUT_OF_STOCK_ORDER_DETAIL_SAVE_FAILED_NO_NUM(0, "erp领料出库详情保存失败,库存不足"),
    ERP_OUT_OF_STOCK_ORDER_DETAIL_UPDATE_FAILED(0, "erp领料出库详情修改失败"),
    ERP_OUT_OF_STOCK_ORDER_DETAIL_DELETE_FAILED(0, "erp领料出库详情删除失败"),
    ERP_OUT_OF_STOCK_ORDER_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED(0, "erp领料出库详情删除失败,调整库存数量失败"),

    /**
     * erp期初库存相关
     */
    ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED(0, "erp期初库存保存失败"),
    ERP_OPENING_INVENTORY_ORDER_SAVE_FAILED_CODE_RE(0, "erp期初库存保存失败,编号重复"),
    ERP_OPENING_INVENTORY_ORDER_UPDATE_FAILED(0, "erp期初库存修改失败"),
    ERP_OPENING_INVENTORY_ORDER_DELETE_FAILED(0, "erp期初库存删除失败"),
    ERP_OPENING_INVENTORY_ORDER_DETAIL_SAVE_FAILED(0, "erp期初库存详情保存失败"),
    ERP_OPENING_INVENTORY_ORDER_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED(0, "erp期初库存详情保存失败,调整库存期初数量失败"),
    ERP_OPENING_INVENTORY_ORDER_DETAIL_UPDATE_FAILED(0, "erp期初库存详情修改失败"),
    ERP_OPENING_INVENTORY_ORDER_DETAIL_DELETE_FAILED(0, "erp期初库存详情删除失败"),
    ERP_OPENING_INVENTORY_ORDER_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED(0, "erp期初库存详情删除失败,调整库存期初数量失败"),
    /**
     * erp原料入库相关
     */
    ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED(0, "erp原料入库保存失败"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_CODE_RE(0, "erp原料入库保存失败，编码重复"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_NO_PU_ORDER(0, "erp原料入库保存失败,没有查到相关的源采购订单信息"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_SAVE_FAILED_NO_COMPANY_ID(0, "erp原料入库保存失败,没有查到相关的源采购中公司信息信息"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_UPDATE_FAILED(0, "erp原料入库修改失败"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DELETE_FAILED(0, "erp原料入库删除失败"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED(0, "erp原料入库详情保存失败"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED(0, "erp原料入库详情保存失败,调整库存信息失败"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_SALE_ORDER(0, "erp原料入库详情保存失败,没有查到相关的源采购订单信息"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_MAIN_ID(0, "erp原料入库详情保存失败,没有关联的的原料入库主订单id"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_SAVE_FAILED_NO_MAIN_ORDER(0, "erp原料入库详情保存失败,没有关联的的原料入库主订单"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_UPDATE_FAILED(0, "erp原料入库详情修改失败"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_DELETE_FAILED(0, "erp原料入库详情删除失败"),
    ERP_REPOSITORY_RECEIPT_RAW_IN_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED(0, "erp原料入库详情删除失败,调整库存信息失败"),

    /**
     * erp成品出库相关
     */
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED(0, "erp成品出库保存失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_CODE_RE(0, "erp成品出库保存失败,编码重复"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_NO_SALE_ORDER(0, "erp成品出库保存失败,没有查到相关的源销售订单信息"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_SAVE_FAILED_NO_COMPANY_ID(0, "erp成品出库保存失败,没有查到相关的源销售订单中公司信息"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_UPDATE_FAILED(0, "erp成品出库修改失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DELETE_FAILED(0, "erp成品出库删除失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED(0, "erp成品出库详情保存失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_UPDATE_STOCK_FAILED(0, "erp成品出库详情保存失败,调整库存失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_STOCK(0, "erp成品出库详情保存失败，库存表里没有该产品，不能出库不存在的产品"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_STOCK_NUM(0, "erp成品出库详情保存失败，库存现有量小于出库数量，不能超额出库产品"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_SALE_ORDER(0, "erp成品出库详情保存失败,没有查到相关的源销售订单信息"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_MAIN_ID(0, "erp成品出库详情保存失败，没有关联的的成品出库主订单id"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_SAVE_FAILED_NO_MAIN_ORDER(0, "erp成品出库详情保存失败，没有关联的的成品出库主订单"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_UPDATE_FAILED(0, "erp成品出库详情修改失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED(0, "erp成品出库详情删除失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED_UPDATE_STOCK_FAILED(0, "erp成品出库详情删除失败，调整库存数量失败"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED_NO_MAIN_ID(0, "erp成品出库详情删除失败,没有关联的的成品出库主订单id"),
    ERP_REPOSITORY_RECEIPT_PRODUCT_OUT_DETAIL_DELETE_FAILED_NO_MAIN_ORDER(0, "erp成品出库详情删除失败，没有关联的的成品出库主订单"),

    /**
     * erp成品入库相关
     */
    ERP_WAREHOUSING_ORDER_SAVE_FAILED(0, "erp成品入库保存失败"),
    ERP_WAREHOUSING_ORDER_SAVE_FAILED_UPDATE_ORDER_ID_TO_TEST_REPORT_FAILED(0, "erp成品入库保存失败,更新成品入库单号到化验单失败"),
    ERP_WAREHOUSING_ORDER_SAVE_FAILED_CODE_RE(0, "erp成品入库保存失败,编号重复"),
    ERP_WAREHOUSING_ORDER_SAVE_FAILED_UPDATE_STOCK_FAILED(0, "erp成品入库保存失败,调整库存失败"),
    ERP_WAREHOUSING_ORDER_UPDATE_FAILED(0, "erp成品入库修改失败"),
    ERP_WAREHOUSING_ORDER_DELETE_FAILED(0, "erp成品入库删除失败"),
    ERP_WAREHOUSING_ORDER_DELETE_FAILED_UPDATE_STOCK_FAILED(0, "erp成品入库删除失败,调整库存失败"),


    /**
     * ERP采购管理
     */
    PURCHASEGOODSORDER_NOT_EXIST(0, "采购收货单不存在"),


    GENERATE_ORDER_ID_FAILED(0, "生成订单号失败 ，请求类型不匹配"),
    /**
     * ERP客户相关
     */
    CONTACT_ADD_FAILED(0, "联系人添加失败"),
    CUSTOMER_CONTACT_RELATION_ADD_FAILED(0, "联系人添加失败"),


    /**
     * 行业相关
     */
    INDUSTRY_SAVE_FAILED(0, "行业信息保存异常"),
    INDUSTRY_UPDATE_FAILED(0, "行业信息修改异常"),
    /**
     * 企业附件相关
     */
    ENTERPRISES_ATTACHMENT_SAVE_FAILED(0, "企业附件保存异常"),
    ENTERPRISES_ATTACHMENT_UPDATE_FAILED(0, "企业附件修改异常"),

    /**
     * 中台系统通知相关
     */
    MIDDLE_SYSTEM_NOTICE_UPDATE_FAILED(0, "中台系统通知修改异常"),
    MIDDLE_SYSTEM_NOTICE_SAVE_FAILED(0, "中台系统通知保存异常"),


    /**
     * crm相关
     */
    CRM_WORK_PROMOTE_ADD_FAILED(0, "crm新增工作推进异常"),
    CRM_WORK_PROMOTE_DELETE_FAILED(0, "crm新增工作推进异常"),
    CRM_COMMERCIAL_OPPORTUNITY_ADD_FAILED(0, "crm新增商机信息异常"),
    UPDATE_CUSTOMER_FAILED(0, "crm更新客户档案异常"),
    UPDATE_CUSTOMER_CONTACT_FAILED(0, "crm更新客户档案联系人异常"),
    UPDATE_CUSTOMER_CONTACT_RELATED_FAILED(0, "crm更新客户档案联系人关联异常"),

    WL_UPDATE_PAY_STATUS_FAILED(0, "物流订单修改付款状态失败"),


    /**
     * 砂石相关
     */
    /**
     * 验货磅单相关
     */
    DINAS_INSPECTION_GOODS_SAVE_FAILED(0,"保存验货磅单失败"),
    DINAS_INSPECTION_GOODS_DELETED_FAILED(0,"删除验货磅单失败"),
    DINAS_INSPECTION_GOODS_DELETED_FAILED_NO_DATA(0,"删除验货磅单失败,没有相关验货磅单数据"),
    DINAS_INSPECTION_GOODS_DELETED_FAILED_HAS_ALREADY(0,"删除验货磅单失败，有已结算过的数据,不能删除已结算过的数据"),
    DINAS_INSPECTION_GOODS_UPDATE_FAILED_HAS_ALREADY(0,"编辑验货磅单失败，有已结算过的数据,不能编辑已结算过的数据"),
    DINAS_INSPECTION_GOODS_DELETED_PURCHASE_FAILED(0,"删除验货磅单,删除采购结算单失败"),
    DINAS_INSPECTION_GOODS_DELETED_SALE_FAILED(0,"删除验货磅单,删除销售结算单失败"),
    DINAS_INSPECTION_GOODS_SAVE_FAILED_DELETE_OLD_PURCHASE_FAILED(0,"保存验货磅单失败,删除旧的采购结算失败"),
    DINAS_INSPECTION_GOODS_SAVE_FAILED_DELETE_OLD_SALE_FAILED(0,"保存验货磅单失败,删除旧的销售结算失败"),
    DINAS_INSPECTION_GOODS_SAVE_PURCHASE_FAILED(0,"保存验货磅单生成采购结算失败"),
    DINAS_INSPECTION_GOODS_SAVE_SALE_FAILED(0,"保存验货磅单生成销售结算失败"),

    /**
     * 采购结算相关
     */

    DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED(0,"修改采购结算单失败"),
    DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_BATCH_FAILED(0,"批量采购结算失败,同一合同号，同一产品名称、规格才可以批量结算"),
    DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_PURCHASE_ORDER_FAILED(0,"修改采购合同单失败"),
    DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_MONEY(0,"可用余额不足，请及时付款"),
    DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_SETTLEMENT_DATA(0,"采购结算结算失败,没有找到相关采购结算数据"),
    DINAS_PURCHASE_SETTLEMENT_SETTLEMENT_FAILED_NO_PURCHASE_DATA(0,"采购结算结算失败,没有找到相关采购合同数据"),
    DINAS_PURCHASE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED(0,"采购结算撤销结算失败"),
    DINAS_PURCHASE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_NO_PURCHASE_ORDER(0,"采购结算撤销结算失败,没有相关采购合同单"),
    DINAS_PURCHASE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_UPDATE_PURCHASE_ORDER_FAILED(0,"采购结算撤销结算失败,修改采购合同单可用金额失败"),

    /**
     * 销售结算相关
     */

    DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED(0,"修改销售结算单失败"),
    DINAS_SALE_SETTLEMENT_SETTLEMENT_BATCH_FAILED(0,"批量销售结算失败,同一合同号，同一产品名称、规格才可以批量结算"),
    DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_SALE_ORDER_FAILED(0,"修改销售合同单失败"),
    DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_MONEY(0,"可用余额不足，请及时付款"),
    DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_SETTLEMENT_DATA(0,"销售结算结算失败,没有找到相关销售结算数据"),
    DINAS_SALE_SETTLEMENT_SETTLEMENT_FAILED_NO_SALE_DATA(0,"销售结算结算失败,没有找到相关销售合同数据"),
    DINAS_SALE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED(0,"销售结算撤销结算失败"),
    DINAS_SALE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_NO_SALE_ORDER(0,"销售结算撤销结算失败,没有相关销售合同单"),
    DINAS_SALE_SETTLEMENT_CANCEL_SETTLEMENT_FAILED_UPDATE_SALE_ORDER_FAILED(0,"销售结算撤销结算失败,修改销售合同单可用金额失败"),



    /**
     * erp采购调价记录相关
     */
    ERP_PURCHASE_ADJUST_ADD_FAILED(0, "erp采购订单调价添加失败"),
    ERP_PURCHASE_ADJUST_DETAIL_ADD_FAILED(0, "erp采购订单调价明细添加失败"),
    ERP_PURCHASE_ADJUST_DELETE_FAILED(0, "erp采购调价删除失败"),
    ERP_PURCHASE_ADJUST_DETAIL_DELETE_FAILED(0, "erp采购调价明细删除失败"),
    ERP_PURCHASE_DETAIL_PRODUCTSPEC_REPEAT(0, "erp采购订单明细产品规格重复"),
    ERP_ADJUST_DETAIL_PRODUCTSPEC_REPEAT(0, "erp采购订单明细产品规格重复"),

    ERP_PURCHASE_USED_BY_PAY(0, "erp采购订单被付款单使用"),
    ERP_PURCHASE_USED_BY_INVOICE(0, "erp采购订单被发票使用"),
    ERP_PURCHASE_USED_BY_SETTLEMENT(0, "erp采购订单被结算单使用"),

    // ERP_PURCHASE_ADJUST_UPDATE_FAILED(0, "erp采购订单明细更新失败"),

    ;

    private Integer code;
    private String message;
}
