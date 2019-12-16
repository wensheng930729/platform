package com.bee.platform.customer.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 客户分类关联表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-31
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ErpCustomerCategoryRelation extends Model<ErpCustomerCategoryRelation> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 客户id
     */
    private Integer customerId;
    /**
     * 客户二级分类id
     */
    private Integer categoryId;
    /**
     * 表示逻辑删除，1-是删除，0-不删除
     */
    private Integer deleted;
    /**
     * 删除时间
     */
    private Date deletedTime;
    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
