package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-04-24
 */

@Data
@NoArgsConstructor
@ToString
@Accessors(chain=true)
@TableName("articles_type")
public class ArticlesType extends Model<ArticlesType> {

    private static final long serialVersionUID = -5055596727016306376L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公共类型名
     */
    private String name;
    /**
     * 企业id
     */
    private Integer orgId;
    /**
     * 0无效，1有效
     */
    private Integer status;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
