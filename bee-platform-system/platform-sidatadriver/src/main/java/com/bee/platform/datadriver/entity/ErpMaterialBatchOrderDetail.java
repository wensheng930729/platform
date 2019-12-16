package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 料批明细表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */


@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_material_batch_order_detail")
public class ErpMaterialBatchOrderDetail extends Model<ErpMaterialBatchOrderDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 料批主表id
     */
    private Integer materialBatchOrderId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;

    /**
     * 产成品批次id
     */
    private Integer productBatchId;

    /**
     * 数量
     */
    private BigDecimal number;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人企业id
     */
    private Integer creatorEnterpriseId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 修改人id
     */
    private Integer modifierId;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
