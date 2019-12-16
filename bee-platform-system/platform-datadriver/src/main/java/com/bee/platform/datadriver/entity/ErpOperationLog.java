package com.bee.platform.datadriver.entity;

import java.io.Serializable;
import java.util.Date;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 操作日志表
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
public class ErpOperationLog extends Model<ErpOperationLog> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 操作的业务id
     */
    private Integer businessId;
    /**
     * 业务类型，从码表取值
     */
    private String businessType;
    /**
     * 操作人
     */
    private Integer operator;
    
    /**
     * 操作人姓名
     */
    private String operatorName;
    /**
     * 操作说明
     */
    private String operateMsg;
    /**
     * 操作时间
     */
    private Date operateTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
    

}
