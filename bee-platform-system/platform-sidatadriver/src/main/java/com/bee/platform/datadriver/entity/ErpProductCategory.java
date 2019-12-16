package com.bee.platform.datadriver.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 产品类别
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class ErpProductCategory extends Model<ErpProductCategory> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 产品分类名字
     */
    private String name;
    /**
     * 状态:1-启用,0-禁用
     */
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    private Integer deleted;
    /**
     * 创建时间
     */
    private Date createTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
