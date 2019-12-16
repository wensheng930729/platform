package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
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
 * 期初库存明细表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_opening_inventory_order_detail")
public class ErpOpeningInventoryOrderDetail extends Model<ErpOpeningInventoryOrderDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 期初库存表id
     */
    private Integer openingInventoryOrderId;
    /**
     * 产品_id
     */
    private Integer productId;
    /**
     * 产品批次id
     */
    private Integer productBatchId;
    /**
     * 产品名称
     */
    private String productName;

    /**
     * 仓库id
     */
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    private String storeHouseName;
    /**
     * 计量单位
     */
    private String unitOfMeasurement;
    /**
     * 化验单
     */
    private String testOrder;
    /**
     * 化验单id
     */
    private Integer testReportId;
    /**
     * 期初数量
     */
    private BigDecimal quantity;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
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
     * 修改人id
     */
    private Integer modifierId;
    /**
     * 修改时间
     */
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
