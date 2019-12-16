package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableName;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 行业分类信息表
 * </p>
 *
 * @author cheng.ke
 * @since 2019-04-24
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@TableName("t_industry")
public class Industry extends Model<Industry> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 父级行业关系
     */
    private Integer pid;
    /**
     * 行业名称
     */
    private String industry;
    /**
     * 子属级别关系
     */
    private Integer level;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
