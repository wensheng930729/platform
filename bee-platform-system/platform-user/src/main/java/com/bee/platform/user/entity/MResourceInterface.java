package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @notes 接口资源关联实体
 * @Author junyang.li
 * @Date 10:16 2019/4/28
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_role_interface")
public class MResourceInterface extends Model<MResourceInterface> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 资源id
     */
    private Integer resourceId;
    /**
     * 接口id
     */
    private Integer interfaceId;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
