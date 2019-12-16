package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @notes 角色
 * @Author junyang.li
 * @Date 17:22 2019/3/4
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("u_roles")
public class Role extends Model<Role> {

    private static final long serialVersionUID = 1L;


    /**
     * 用户角色id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 用户角色名称
     */
    private String name;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
