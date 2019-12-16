package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * <p>
 * 后台管理用户列表
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-28
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_platform_managers")
public class PlatformManagers extends Model<PlatformManagers> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "manager_id", type = IdType.AUTO)
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
     * 密码
     */
    private String password;
    /**
     * 姓名
     */
    private String nickname;
    /**
     * 创建时间
     */
    private Date createAt;
    /**
     * 修改时间
     */
    private Date updateAt;
    /**
     * 账户说明
     */
    private String notes;
    /**
     * 账户状态
     */
    private Integer status;
    /**
     * 修改人id
     */
    private Integer creatorId;
    /**
     * 修改人id
     */
    private Integer updateId;

    @Override
    protected Serializable pkVal() {
        return this.managerId;
    }

}
