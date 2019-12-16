package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 库存表
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-03
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_stock")
public class ErpStock extends Model<ErpStock> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer orgId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 化验单id
     */
    private Integer testReportId;
    /**
     * 化验单编号
     */
    private String testReportCode;

    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产成品批次id
     */
    private Integer productBatchId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品类别id
     */
    private Integer productCategoryId;
    /**
     * 产品类别
     */
    private String productCategory;
    /**
     * 单位
     */
    private String unit;
    /**
     * 仓库id
     */
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    private String repositoryName;
    /**
     * 期初数量
     */
    private BigDecimal initNum;
    /**
     * 入库数量
     */
    private BigDecimal inStockNum;
    /**
     * 出库数量
     */
    private BigDecimal outStockNum;
    /**
     * 盘点差量，正负值
     */
    private BigDecimal dNum;
    /**
     * 现存数量
     */
    private BigDecimal stockNum;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
