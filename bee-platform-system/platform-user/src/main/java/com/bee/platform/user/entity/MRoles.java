package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @notes 管理员角色表
 * @Author junyang.li
 * @Date 10:14 2019/4/28
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_roles")
public class MRoles extends Model<MRoles> {

    private static final long serialVersionUID = 1L;

    /**
     * 角色id
     */
    @TableId(value = "role_id", type = IdType.AUTO)
    private Integer roleId;
    /**
     * 角色名称
     */
    private String roleName;
    /**
     * 角色类型
     */
    private Integer roleType;
    /**
     * 创建企业
     */
    private String createCompanyId;
    /**
     * 角色状态
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人姓名
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人姓名
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;

    @Override
    protected Serializable pkVal() {
        return this.roleId;
    }
}
