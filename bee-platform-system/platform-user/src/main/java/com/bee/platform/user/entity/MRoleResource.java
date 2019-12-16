package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
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
 * @notes 角色资源关联表
 * @Author junyang.li
 * @Date 10:15 2019/4/28
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_role_resource")
public class MRoleResource extends Model<MRoleResource> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 角色id
     */
    private Integer roleId;
    /**
     * 资源id
     */
    private Integer resourceId;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
