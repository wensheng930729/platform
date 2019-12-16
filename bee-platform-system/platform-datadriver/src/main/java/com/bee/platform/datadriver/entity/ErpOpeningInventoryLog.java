package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 期初库存日志表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_opening_inventory_log")
public class ErpOpeningInventoryLog extends Model<ErpOpeningInventoryLog> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 期初库存主表id
     */
    private Integer openingInventoryOrderId;
    /**
     * 操作员id
     */
    private Integer operatorId;
    /**
     * 操作员名称
     */
    private String operatorName;
    /**
     * 操作时间
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date operateTime;
    /**
     * 操作内容
     */
    private String operateContent;
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


    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
