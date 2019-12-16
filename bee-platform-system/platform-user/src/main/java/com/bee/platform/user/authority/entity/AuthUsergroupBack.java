package com.bee.platform.user.authority.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 用户组
 * </p>
 *
 * @author ll
 * @since 2019-08-9
 */
@Data
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_usergroup_back")
public class AuthUsergroupBack extends Model<AuthUsergroupBack> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 用户组名
     */
    private String groupName;
    /**
     * 描述
     */
    private String description;
    /**
     * 是否默认 0否 1是
     */
    private Integer defaultValue;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 逻辑删除，0-否 1-是
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
