package com.bee.platform.common.entity;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @description: 工业云平台后台用户登录实体
 * @author: junyang.li
 * @create: 2018-12-11 18:15
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class ManagerInfo implements Serializable {

    /**
     * id
     */
    private Integer managerId;
    /**
     * 管理员业务id
     */
    private String beesrvId;
    /**
     * 账户
     */
    private String username;
    /**
     * 手机号
     */
    private String phone;
    /**
     * 系统登录凭证
     */
    private String sysToken;
    /**
     * token失效时间
     */
    private Date expiresIn;
    /**
     * 头像地址
     */
    private String head;
    /**
     * 邮箱
     */
    private String email;
    /**
     * 姓名
     */
    private String nickname;
    /**
     * 账户说明
     */
    private String notes;
    /**
     * 账户状态
     */
    private Integer status;
    /**
     *  角色集
     */
    private MRoleInfo roleInfo;
    /**
     * 最后修改时间
     */
    private Date updateTime;
    /**
     * 最后操作人id
     */
    private Integer updateId;
    /**
     * 最后操作人账号名
     */
    private String updateName;
}
