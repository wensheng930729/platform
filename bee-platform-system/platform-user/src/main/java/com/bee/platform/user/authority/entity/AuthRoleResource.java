package com.bee.platform.user.authority.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 资源角色表
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("auth_role_resource")
public class AuthRoleResource extends Model<AuthRoleResource> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 菜单id
     */
    private Integer resourceId;
    /**
     * 菜单序号
     */
    private Integer orderNum;
    /**
     * 是否删除0未删除1已删除
     */
    private Integer deleted;
    /**
     * 创建人
     */
    private Integer createUser;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
