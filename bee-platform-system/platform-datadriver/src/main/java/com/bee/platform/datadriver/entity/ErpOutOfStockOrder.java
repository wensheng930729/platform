package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 领料出库主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_out_of_stock_order")
public class ErpOutOfStockOrder extends Model<ErpOutOfStockOrder> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 领料出库编号
     */
    private String code;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 出库日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date outOfStockTime;
    /**
     * 料批id
     */
    private Integer materialBatchId;
    /**
     * 料批名称
     */
    private String materialBatchName;
    /**
     * 炉号
     */
    private String furnaceNumber;
    /**
     * 炉号id
     */
    private Integer furnaceNumberId;
    /**
     * 班次
     */
    private String classes;
    /**
     * 备注
     */
    private String remark;
    /**
     * 机械生产时间
     */
    private String machineProductionTime;
    /**
     * 电炉生产时间
     */
    private String electricFurnaceProductionTime;
    /**
     * 有功电量
     */
    private BigDecimal activeElectricity;
    /**
     * 无功电量
     */
    private BigDecimal kvarh;
    /**
     * 确认状态(0已保存，1已确认)
     */
    private Integer state;
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



    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
