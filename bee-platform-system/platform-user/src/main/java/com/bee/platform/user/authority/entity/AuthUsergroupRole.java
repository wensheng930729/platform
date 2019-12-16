package com.bee.platform.user.authority.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 用户组与角色/功能/应用的关联表
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
@Data
@ToString
@NoArgsConstructor
@Accessors(chain=true)
public class AuthUsergroupRole extends Model<AuthUsergroupRole> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 用户组id
     */
    private Integer usergroupId;
    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 角色父id
     */
    private Integer pid;
    /**
     * 是否启用：1启用，0禁用
     */
    private Integer status;
    /**
     * 子系统标识
     */
    private String subSys;
    /**
     * 该用户下角色级别：与角色表的level对应
     */
    private Integer level;
    /**
     * 该用户下角色或功能的自定义分类，基于level字段的一个分类，用于分类展示
     */
    private String roleType;
    /**
     * 该用户下角色的序号
     */
    private Integer orderNum;
    /**
     * 标识 1勾选 0未勾选
     */
    private Integer flag;
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
     * 逻辑删除，1-是，0-否
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
