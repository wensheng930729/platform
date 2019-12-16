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
 * 领料出库明细表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_out_of_stock_order_detail")
public class ErpOutOfStockOrderDetail extends Model<ErpOutOfStockOrderDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 领料出库主表id
     */
    private Integer outOfStockOrderId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;

    /**
     * 仓库id
     */
    private Integer repositoryId;
    /**
     * 仓库
     */
    private String storehouse;
    /**
     * 计量单位
     */
    private String unit;
    /**
     * 化验单id
     */
    private Integer testReportId;
    /**
     * 化验单编号
     */
    private String testReportCode;

    /**
     * 采购批次
     */
    private String purchaseBatch;
    /**
     * 出库数量
     */
    private BigDecimal number;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人企业id
     */
    private Integer creatorEnterpriseId;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 修改人id
     */
    private Integer modifierId;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 创建时间
     */
    private Date createTime;

    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
