package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 操作日志表
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("erp操作日志请求rq")
public class ErpOperationLogRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 操作的业务id
     */
    private Integer businessId;
    /**
     * 业务类型，从码表取值
     */
    private String businessType;

}
