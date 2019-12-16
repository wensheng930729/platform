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
 * @notes 用户登录凭证
 * @Author junyang.li
 * @Date 17:21 2019/3/4
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("user_tokens")
public class UserToken extends Model<UserToken> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 用户账号
     */
    private String username;
    /**
     * 子系统token
     */
    private String sysToken;
    /**
     * 子系统token失效时间
     */
    private Date expiresIn;
    /**
     * 当前登录的企业id
     */
    private Integer nowCompany;
    /**
     * 创建人
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人
     */
    private String updateUser;
    /**
     * 修改时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    public UserToken(String username, Date expiresIn) {
        this.username = username;
        this.expiresIn = expiresIn;
    }

}
