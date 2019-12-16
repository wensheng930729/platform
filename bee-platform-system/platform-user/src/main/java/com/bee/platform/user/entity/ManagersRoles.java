package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @notes 管理员角色关联表
 * @Author junyang.li
 * @Date 10:53 2019/3/22
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_managers_roles")
public class ManagersRoles extends Model<ManagersRoles> {

    private static final long serialVersionUID = 1L;
    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 管理员id
     */
    private Integer managerId;
    /**
     * 角色id
     */
    private Integer roleId;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
